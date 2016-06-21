require(RPostgreSQL)
require(maptools)
require(data.table)
require(doMC)

drv <- dbDriver("PostgreSQL")  #Specify a driver for postgreSQL type database
con <- dbConnect(drv, dbname="qaeco_spatial", user="qaeco", password="Qpostgres15", host="boab.qaeco.com", port="5432")  #Connection to database server on Boab


coll_db_hm <- as.data.table(dbGetQuery(con,"
  SELECT
    pts.id AS pid, grid.id AS id, pts.hour AS hour, pts.day AS day, pts.month AS month, pts.year as year, pts.x AS x, pts.y AS y
  FROM
    vline.vic_gda9455_fauna_egkcoll_train_onnetwork AS pts, gis_victoria.vic_gda9455_admin_state_1kmgrid AS grid
  WHERE
    ST_contains(grid.geom, pts.geom)
  "))

spatial <- as.matrix(cbind(x=145,y=-37))

for (i in 1:nrow(coll_db_hm)){
  coll_db_hm[i,dawn:=(crepuscule(spatial,as.POSIXct(paste0("2012-",month,"-15")),solarDep=6,direction=c("dawn"),POSIXct.out=FALSE)*24)]
  coll_db_hm[i,dusk:=(crepuscule(spatial,as.POSIXct(paste0("2012-",month,"-15")),solarDep=6,direction=c("dusk"),POSIXct.out=FALSE)*24)]
  coll_db_hm[i,dawndusk:=(crepuscule(spatial,as.POSIXct(paste0("2012-",month,"-15")),solarDep=6,direction=c("dusk"),POSIXct.out=FALSE)*24)-(crepuscule(spatial,as.POSIXct(paste0("2012-",month,"-15")),solarDep=6,direction=c("dawn"),POSIXct.out=FALSE)*24)]
}
setkey(coll_db_hm,id,hour,dawndusk)


coll_db_bgh <- as.data.table(dbGetQuery(con,"
  SELECT
    y.id AS id, y.egk AS egk, AVG(y.trains) AS trains, AVG(y.length) AS length, AVG(y.speed) AS speed, y.hour AS hour, CAST(0 AS integer) AS coll
  FROM
    (SELECT
      x.id AS id, x.egk AS egk, COUNT(x.trains) AS trains, AVG(x.length) AS length, AVG(x.speed) AS speed, x.hour AS hour
    FROM
      (SELECT 
          grid.id AS id, grid.egk AS egk, seg.dow as dow, COUNT(seg.train) AS trains, ST_Length(ST_LineMerge(ST_Union(ST_Intersection(grid.geom, seg.geom))))/1000 AS length, AVG(seg.speed) AS speed, seg.hour AS hour
        FROM
          vline.vic_gda9455_rail_vline_speeds AS seg, 
          (SELECT 
            g.id as id,
            ST_Value(p.rast,ST_Centroid(g.geom)) AS egk,
            g.geom as geom
          FROM 
            gis_victoria.vic_gda9455_grid_egk_preds_brt AS p,
            gis_victoria.vic_gda9455_admin_state_1kmgrid AS g
          WHERE
            ST_Intersects(p.rast,ST_Centroid(g.geom))) AS grid
        WHERE
          ST_Intersects(grid.geom, seg.geom)
        AND
          grid.egk NOTNULL
        AND
          seg.speed <> 'Infinity'
        AND
          seg.speed <= 160
        GROUP BY
          grid.id, grid.egk, seg.train, seg.hour, seg.dow
        ) as x
    GROUP BY
      x.id, x.egk, x.hour, x.dow) as y
   GROUP BY
    y.id, y.egk, y.hour
    "))
setkey(coll_db_bgh,id,hour)

#write.csv(coll_db_bgh,"data/coll_db_bgh")
#coll_db_bgh <- as.data.table(read.csv("data/coll_db_bgh"))

registerDoMC(detectCores() - 1)
coll_db_bghm <- foreach (i = 1:12, .combine=rbind) %dopar% {
  cbind(coll_db_bgh, "dawn"=(crepuscule(spatial,as.POSIXct(paste0("2012-",as.integer(sprintf("%02d",i)),"-15")),solarDep=6,direction=c("dawn"),POSIXct.out=FALSE)*24), "dusk"=(crepuscule(spatial,as.POSIXct(paste0("2012-",as.integer(sprintf("%02d",i)),"-15")),solarDep=6,direction=c("dusk"),POSIXct.out=FALSE)*24), "dawndusk"=(crepuscule(spatial,as.POSIXct(paste0("2012-",as.integer(sprintf("%02d",i)),"-15")),solarDep=6,direction=c("dusk"),POSIXct.out=FALSE)*24)-(crepuscule(spatial,as.POSIXct(paste0("2012-",as.integer(sprintf("%02d",i)),"-15")),solarDep=6,direction=c("dawn"),POSIXct.out=FALSE)*24))
}
setkey(coll_db_bghm,id,hour,dawndusk)

model.data.hm <- coll_db_bghm
c <- coll_db_hm[model.data.hm,.N,by=c("id","hour","dawndusk"),nomatch=0]
setkey(c,id,hour,dawndusk)

model.data.hm[c, coll := i.N]

nomatch <- coll_db_hm[!model.data.hm]

d <- function (h, peak, trough, l) {
  # evaluate a two-Gaussian function for given peak/trough/lengthscale
  exp(-((h - peak) / l) ^ 2) -   exp(-((h - trough) / l) ^ 2)
}

dawn.or.dusk <- function (h, dawn = 6, dusk = 18, slope = 2) {
  # compose a two-Gaussian curve, for day/night
  # dawn and dusk or dusk and dawn
  
  # get inner and outer differences
  diff_inner <- (dusk - dawn) / slope
  diff_outer <- ((24 - dusk) + dawn) / slope
  
  ifelse(h < dawn,
         d(h, dawn, dusk - 24, diff_outer),
         ifelse(h < dusk,
                d(h, dawn, dusk, diff_inner),
                d(h, dawn + 24, dusk, diff_outer)))
}

model.data.hm$light <- sin((2 * pi * (model.data.hm$hour - 6)) / 24) # ambient light intensity

model.data.hm$light2 <- model.data.hm$light ^ 2

model.data.hm$dawnordusk <- dawn.or.dusk(h=model.data.hm$hour,dawn=model.data.hm$dawn,dusk=model.data.hm$dusk)


write.csv(model.data.hm, file = "data/model_data_hm.csv", row.names=FALSE)


################Hexagonal Grid#####################
coll_db_hm <- as.data.table(dbGetQuery(con,"
  SELECT
    pts.id AS pid, grid.id AS id, pts.hour AS hour, pts.day AS day, pts.month AS month, pts.year as year, pts.x AS x, pts.y AS y
  FROM
    vline.vic_gda9455_fauna_egkcoll_train_onnetwork AS pts, gis_victoria.vic_gda9455_admin_state_1km2hexgrid AS grid
  WHERE
    ST_contains(grid.geom, pts.geom)
  "))

coll_db_bgh <- as.data.table(dbGetQuery(con,"
    SELECT
      x.id AS id, x.egk AS egk, AVG(x.train) AS trains, AVG(x.speed) AS speed, x.hour AS hour, CAST(SUM(x.coll) AS integer) AS coll
    FROM
      (SELECT 
          grid.id AS id, grid.egk AS egk, COUNT(seg.train)*ST_Length(ST_LineMerge(ST_Union(ST_Intersection(grid.geom, seg.geom))))/1000 AS TRAIN, AVG(seg.speed) AS speed, seg.hour AS hour, CAST(0 AS integer) AS coll
        FROM
          vline.vic_gda9455_rail_vline_speeds AS seg, 
          (SELECT 
            g.id as id,
            ST_Value(p.rast,ST_Centroid(g.geom)) AS egk,
            g.geom as geom
          FROM 
            gis_victoria.vic_gda9455_grid_egk_preds_brt AS p,
            gis_victoria.vic_gda9455_admin_state_1km2hexgrid AS g
          WHERE
            ST_Intersects(p.rast,ST_Centroid(g.geom))) AS grid
        WHERE
          ST_Intersects(grid.geom, seg.geom)
        AND
          grid.egk NOTNULL
        AND
          seg.speed <> 'Infinity'
        AND
          seg.speed <= 160
        GROUP BY
          grid.id, grid.egk, seg.train, seg.hour
        ) as x
    GROUP BY
      x.id, x.egk, x.hour
    "))
setkey(coll_db_bgh,id,hour)

write.csv(model.data.hm, file = "data/model_data_hm_hex.csv", row.names=FALSE)
