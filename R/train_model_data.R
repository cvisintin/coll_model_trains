require(RPostgreSQL)
require(maptools)
require(data.table)
require(doMC)
require(rgdal)
require(sp)

drv <- dbDriver("PostgreSQL")  #Specify a driver for postgreSQL type database
con <- dbConnect(drv, dbname="qaeco_spatial", user="qaeco", password="Qpostgres15", host="boab.qaeco.com", port="5432")  #Connection to database server on Boab


coll_db_hm <- as.data.table(dbGetQuery(con,"
  SELECT
    pts.id AS pid, grid.id AS id, pts.hour AS hour, pts.day AS day, pts.month AS month, pts.year as year, ST_X(ST_Centroid(grid.geom)) AS x, ST_Y(ST_Centroid(grid.geom)) AS y
  FROM
    vline.vic_gda9455_fauna_egkcoll_train_onnetwork AS pts, gis_victoria.vic_gda9455_admin_state_1kmgrid AS grid
  WHERE
    ST_contains(grid.geom, pts.geom)
  "))

png('figs/coll_hour.png', pointsize = 18, res=100, width = 1100, height = 600, bg='transparent')
par(mgp=c(1.8,0.5,0),mar=c(3.0,3.0,3,1), cex.axis=0.8)
hour_hist <- hist(coll_db_hm$hour, breaks=0:24, xlab= 'Hour', xaxt="n", xaxs="i", yaxs="i", col='grey', main='Collisions by Hour of Day', font.main = 1)
axis(side=1,at=hour_hist$mids,labels=seq(0,23))
dev.off()

png('figs/coll_month.png', pointsize = 21, res=100, width = 800, height = 600, bg='transparent')
par(mgp=c(1.8,0.5,0),mar=c(3.0,3.0,3,1), cex.axis=0.8)
month_hist <- hist(coll_db_hm$month, breaks=0:12, xlab= 'Month', xaxt="n", xaxs="i", yaxs="i", col='grey', main='Collisions by Month', font.main = 1)
axis(side=1,at=month_hist$mids,labels=seq(1,12))
dev.off()


for (i in 1:nrow(coll_db_hm)){
  coords <- coll_db_hm[i,.(x,y)]
  coordinates(coords) <- c("x", "y")
  proj4string(coords) <- CRS("+init=epsg:28355")
  spatial <- spTransform(coords, CRS("+init=epsg:4326"))
  coll_db_hm[i,dawn:=(crepuscule(spatial,as.POSIXct(paste0("2012-",month,"-15")),solarDep=6,direction=c("dawn"),POSIXct.out=FALSE)*24)]
  coll_db_hm[i,dusk:=(crepuscule(spatial,as.POSIXct(paste0("2012-",month,"-15")),solarDep=6,direction=c("dusk"),POSIXct.out=FALSE)*24)]
  coll_db_hm[i,dawndusk:=(crepuscule(spatial,as.POSIXct(paste0("2012-",month,"-15")),solarDep=6,direction=c("dusk"),POSIXct.out=FALSE)*24)-(crepuscule(spatial,as.POSIXct(paste0("2012-",month,"-15")),solarDep=6,direction=c("dawn"),POSIXct.out=FALSE)*24)]
}
setkey(coll_db_hm,id,hour,dawndusk)

####################Sunset/sunrise variation across network#################################
network.xy <- as.data.table(dbGetQuery(con,paste0("
  SELECT 
    g.id AS id, ST_X(ST_Centroid(g.geom)) AS x, ST_Y(ST_Centroid(g.geom)) AS y
  FROM
    vline.vic_gda9455_rail_vline_speeds AS seg,
    gis_victoria.vic_gda9455_admin_state_1kmgrid AS g
  WHERE
    ST_Intersects(g.geom, seg.geom)
  GROUP BY
    g.id, g.geom
    ")))

for (i in 1:nrow(network.xy)){
  coords <- network.xy[i,.(x,y)]
  coordinates(coords) <- c("x", "y")
  proj4string(coords) <- CRS("+init=epsg:28355")
  spatial <- spTransform(coords, CRS("+init=epsg:4326"))
  network.xy[i,dawn:=(crepuscule(spatial,as.POSIXct(paste0("2012-07-15")),solarDep=6,direction=c("dawn"),POSIXct.out=FALSE)*24)]
  network.xy[i,dusk:=(crepuscule(spatial,as.POSIXct(paste0("2012-07-15")),solarDep=6,direction=c("dusk"),POSIXct.out=FALSE)*24)]
}
setkey(network.xy,id)

range(network.xy$dawn)
range(network.xy$dusk)

#####################################################################################################

# registerDoMC(detectCores() - 1)
# coll_db_bgh <- foreach (i = c(0,70000,140000,210000,280000,350000,420000), .combine=rbind) %dopar% {
#   drv <- dbDriver("PostgreSQL")  #Specify a driver for postgreSQL type database
#   con <- dbConnect(drv, dbname="qaeco_spatial", user="qaeco", password="Qpostgres15", host="boab.qaeco.com", port="5432")  #Connection to database server on Boab
#   as.data.table(dbGetQuery(con,paste0("
#   SELECT
#     grid3.id AS id, grid3.egk AS egk, AVG(grid3.trains) AS trains, AVG(grid3.length) AS length, AVG(grid3.speed) AS speed, grid3.hour AS hour, CAST(0 AS integer) AS coll, grid3.x AS x, grid3.y AS y
#   FROM
#     (SELECT
#       grid2.id AS id, grid2.egk AS egk, COUNT(grid2.trains) AS trains, AVG(grid2.length) AS length, AVG(grid2.speed) AS speed, grid2.hour AS hour, grid2.x AS x, grid2.y AS y
#     FROM
#       (SELECT
#           grid.id AS id, grid.egk AS egk, seg.dow as dow, COUNT(seg.train) AS trains, ST_Length(ST_LineMerge(ST_Union(ST_Intersection(grid.geom, seg.geom))))/1000 AS length, AVG(seg.speed) AS speed, seg.hour AS hour, grid.x AS X, grid.y AS Y
#         FROM
#           vline.vic_gda9455_rail_vline_speeds AS seg,
#           (SELECT
#             g.id as id,
#             ST_Value(p.rast,ST_Centroid(g.geom)) AS egk,
#             ST_X(ST_Centroid(g.geom)) AS x,
#             ST_Y(ST_Centroid(g.geom)) AS y,
#             g.geom as geom
#           FROM
#             gis_victoria.vic_gda9455_grid_egk_preds_brt AS p,
#             gis_victoria.vic_gda9455_admin_state_1kmgrid AS g
#           WHERE
#             (g.id >= ",i," AND g.id < ",i+70000,")
#           AND
#             p.rast && g.geom
#           AND
#             ST_Intersects(p.rast,ST_Centroid(g.geom))
#           ORDER BY g.id
#           LIMIT 70000) AS grid
#         WHERE
#           ST_Intersects(grid.geom, seg.geom)
#         AND
#           grid.egk NOTNULL
#         AND
#           seg.speed <> 'Infinity'
#         AND
#           seg.speed <= 160
#         AND
#           seg.ogc_fid NOT IN ('455770', '455788', '455806', '477632', '477765', '784862', '784880', '784898', '806602','806735',
#                               '1103153', '1103171', '1103189', '1124893', '1125026', '1504072', '1504090', '1504108', '1534625',
#                               '1534758', '1980999', '1981017', '1981035', '2014506','2014639')
#         GROUP BY
#           grid.id, grid.egk, seg.train, seg.hour, seg.dow, grid.x, grid.y
#         ) as grid2
#     GROUP BY
#       grid2.id, grid2.egk, grid2.hour, grid2.dow, grid2.x, grid2.y) as grid3
#    GROUP BY
#     grid3.id, grid3.egk, grid3.hour, grid3.x, grid3.y
#     ")))
# }
# setkey(coll_db_bgh,id,hour)

###################################

registerDoMC(detectCores() - 1)
coll_db_bgh <- foreach (i = c(0,70000,140000,210000,280000,350000,420000), .combine=rbind) %dopar% {
  drv <- dbDriver("PostgreSQL")  #Specify a driver for postgreSQL type database
  con <- dbConnect(drv, dbname="qaeco_spatial", user="qaeco", password="Qpostgres15", host="boab.qaeco.com", port="5432")  #Connection to database server on Boab
  as.data.table(dbGetQuery(con,paste0("
                                      SELECT
                                      grid2.id AS id, grid2.egk AS egk, AVG(grid2.trains) AS trains, AVG(grid2.length) AS length, AVG(grid2.speed) AS speed, grid2.hour AS hour, CAST(0 AS integer) AS coll, grid2.x AS x, grid2.y AS y
                                      FROM
                                      (SELECT
                                      grid.id AS id, grid.egk AS egk, seg.dow as dow, COUNT(seg.train) AS trains, ST_Length(ST_LineMerge(ST_Union(ST_Intersection(grid.geom, seg.geom))))/1000 AS length, AVG(seg.speed) AS speed, seg.hour AS hour, grid.x AS X, grid.y AS Y
                                      FROM
                                      vline.vic_gda9455_rail_vline_speeds AS seg,
                                      (SELECT
                                      g.id as id,
                                      ST_Value(p.rast,ST_Centroid(g.geom)) AS egk,
                                      ST_X(ST_Centroid(g.geom)) AS x,
                                      ST_Y(ST_Centroid(g.geom)) AS y,
                                      g.geom as geom
                                      FROM
                                      gis_victoria.vic_gda9455_grid_egk_preds_brt AS p,
                                      gis_victoria.vic_gda9455_admin_state_1kmgrid AS g
                                      WHERE
                                      (g.id >= ",i," AND g.id < ",i+70000,")
                                      AND
                                      p.rast && g.geom
                                      AND
                                      ST_Intersects(p.rast,ST_Centroid(g.geom))
                                      ORDER BY g.id
                                      LIMIT 70000) AS grid
                                      WHERE
                                      ST_Intersects(grid.geom, seg.geom)
                                      AND
                                      grid.egk NOTNULL
                                      AND
                                      seg.speed <> 'Infinity'
                                      AND
                                      seg.speed <= 160
                                      AND
                                      seg.ogc_fid NOT IN ('455770', '455788', '455806', '477632', '477765', '784862', '784880', '784898', '806602','806735',
                                      '1103153', '1103171', '1103189', '1124893', '1125026', '1504072', '1504090', '1504108', '1534625',
                                      '1534758', '1980999', '1981017', '1981035', '2014506','2014639')
                                      GROUP BY
                                      grid.id, grid.egk, seg.train, seg.hour, seg.dow, grid.x, grid.y
                                      ) as grid2
                                      GROUP BY
                                      grid2.id, grid2.egk, grid2.hour, grid2.x, grid2.y
                                      ")))
}
setkey(coll_db_bgh,id,hour)

###################################

#write.csv(coll_db_bgh,"data/coll_db_bgh", row.names=FALSE)
#coll_db_bgh <- as.data.table(read.csv("data/coll_db_bgh"))

registerDoMC(detectCores() - 1)
coll_db_bghm <- foreach (i = 1:12, .combine=rbind) %dopar% {
  coords <- coll_db_bgh[,.(x,y)]
  coordinates(coords) <- c("x", "y")
  proj4string(coords) <- CRS("+init=epsg:28355")
  spatial <- spTransform(coords, CRS("+init=epsg:4326"))
  cbind(coll_db_bgh,
    "dawn"=(crepuscule(spatial,as.POSIXct(paste0("2012-",as.integer(sprintf("%02d",i)),"-15")),solarDep=6,direction=c("dawn"),POSIXct.out=FALSE)*24),
    "dusk"=(crepuscule(spatial,as.POSIXct(paste0("2012-",as.integer(sprintf("%02d",i)),"-15")),solarDep=6,direction=c("dusk"),POSIXct.out=FALSE)*24),
    "dawndusk"=(crepuscule(spatial,as.POSIXct(paste0("2012-",as.integer(sprintf("%02d",i)),"-15")),solarDep=6,direction=c("dusk"),POSIXct.out=FALSE)*24)-(crepuscule(spatial,as.POSIXct(paste0("2012-",as.integer(sprintf("%02d",i)),"-15")),solarDep=6,direction=c("dawn"),POSIXct.out=FALSE)*24)
    )
}
setkey(coll_db_bghm,id,hour,dawndusk)

model.data.hm <- copy(coll_db_bghm)
c <- coll_db_hm[model.data.hm, .N, by=c("id","hour","dawndusk"), nomatch=0]
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


################ Experimental Hexagonal Grid #####################
# coll_db_hm <- as.data.table(dbGetQuery(con,"
#   SELECT
#     pts.id AS pid, grid.id AS id, pts.hour AS hour, pts.day AS day, pts.month AS month, pts.year as year, pts.x AS x, pts.y AS y
#   FROM
#     vline.vic_gda9455_fauna_egkcoll_train_onnetwork AS pts, gis_victoria.vic_gda9455_admin_state_1km2hexgrid AS grid
#   WHERE
#     ST_contains(grid.geom, pts.geom)
#   "))
# 
# coll_db_bgh <- as.data.table(dbGetQuery(con,"
#   SELECT
#     y.id AS id, y.egk AS egk, AVG(y.trains) AS trains, AVG(y.length) AS length, AVG(y.speed) AS speed, y.hour AS hour, CAST(0 AS integer) AS coll
#   FROM
#     (SELECT
#       x.id AS id, x.egk AS egk, COUNT(x.trains) AS trains, AVG(x.length) AS length, AVG(x.speed) AS speed, x.hour AS hour
#     FROM
#       (SELECT 
#         grid.id AS id, grid.egk AS egk, seg.dow as dow, COUNT(seg.train) AS trains, ST_Length(ST_LineMerge(ST_Union(ST_Intersection(grid.geom, seg.geom))))/1000 AS length, AVG(seg.speed) AS speed, seg.hour AS hour
#       FROM
#         vline.vic_gda9455_rail_vline_speeds AS seg, 
#         (SELECT 
#           g.id as id,
#           ST_Value(p.rast,ST_Centroid(g.geom)) AS egk,
#           g.geom as geom
#         FROM 
#           gis_victoria.vic_gda9455_grid_egk_preds_brt AS p,
#           gis_victoria.vic_gda9455_admin_state_1km2hexgrid AS g
#         WHERE
#           ST_Intersects(p.rast,ST_Centroid(g.geom))) AS grid
#       WHERE
#         ST_Intersects(grid.geom, seg.geom)
#       AND
#         grid.egk NOTNULL
#       AND
#         seg.speed <> 'Infinity'
#       AND
#         seg.speed <= 160
#       GROUP BY
#         grid.id, grid.egk, seg.train, seg.hour, seg.dow
#       ) as x
#     GROUP BY
#       x.id, x.egk, x.hour, x.dow) as y
#   GROUP BY
#     y.id, y.egk, y.hour
#     "))
# setkey(coll_db_bgh,id,hour)
# 
# write.csv(model.data.hm, file = "data/model_data_hm_hex.csv", row.names=FALSE)
