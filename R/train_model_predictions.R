require(data.table)
require(maptools)
require(doMC)
require(RPostgreSQL)
require(rstan)

n <- nrow(model.data.bin)
y <- model.data.bin$coll
egk <- model.data.bin$egk
trains <- model.data.bin$trains
speed <- model.data.bin$speed

spatial <- as.matrix(cbind(x=145,y=-37))

model.coefs <- get_posterior_mean(coll.stan.bin2, pars="b")
model.coefs.ci <- summary(coll.stan.bin2, pars = c("b"), probs = c(0.025, 0.975))$summary[, c("2.5%", "97.5%")]

pred.values <- expand.grid(h=c(6,12,18),m=c(1,4,7))

registerDoMC(detectCores() - 1)

y_pred_rate <- foreach(i = 1:nrow(pred.values), .combine=rbind) %dopar% {
  dawn <- crepuscule(spatial,as.POSIXct(paste0("2012-",pred.values[i,2],"-15")),solarDep=6,direction=c("dawn"),POSIXct.out=FALSE)*24
  dusk <- crepuscule(spatial,as.POSIXct(paste0("2012-",pred.values[i,2],"-15")),solarDep=6,direction=c("dusk"),POSIXct.out=FALSE)*24
  light <- sin((2 * pi * (pred.values[i,1] - 6)) / 24)
  light2 <- sin((2 * pi * (pred.values[i,1] - 6)) / 24) * sin((2 * pi * (pred.values[i,1] - 6)) / 24)
  diff_inner <- (dusk - dawn) / 2
  diff_outer <- ((24 - dusk) + dawn) / 2
  if (pred.values[i,1] < dawn) {
    dawnordusk <- exp(-((pred.values[i,1] - dawn) / diff_outer) ^ 2) - exp(-((pred.values[i,1] - (dusk - 24)) / diff_outer) ^ 2)
  }
  if (pred.values[i,1] < dusk) {
    dawnordusk <- exp(-((pred.values[i,1] - dawn) / diff_inner) ^ 2) - exp(-((pred.values[i,1] - dusk) / diff_inner) ^ 2)
  }else{
    dawnordusk <- exp(-((pred.values[i,1] - (dawn + 24)) / diff_outer) ^ 2) - exp(-((pred.values[i,1] - dusk) / diff_outer) ^ 2)
  }
  
  preds <- exp(model.coefs[1] + model.coefs[2]*log(egk) + model.coefs[3]*(log(3.86)-mean(log(trains))) + 0 + model.coefs[5]*light + model.coefs[6]*light2 + model.coefs[7]*dawnordusk + log(1))
  dt <- as.data.table(cbind("id"=model.data.bin$id,"coll_rate"=preds,"hour"=rep(pred.values[i,1],length(model.data.bin$id)),"month"=rep(pred.values[i,2],length(model.data.bin$id))))
  unique(dt)
}

write.csv(y_pred_rate,"output/coll_rate_preds.csv")

drv <- dbDriver("PostgreSQL")  #Specify a driver for postgreSQL type database
con <- dbConnect(drv, dbname="qaeco_spatial", user="qaeco", password="Qpostgres15", host="boab.qaeco.com", port="5432")  #Connection to database server on Boab

dbWriteTable(con, c("vline","egk_coll_rate_glm"), value=y_pred_rate, overwrite=TRUE, row.names=FALSE)

system(
  paste0('pgsql2shp -f /home/casey/Research/Github/coll_model_trains/output/coll_rate_preds -h boab.qaeco.com -p 5432 -u qaeco -P Qpostgres15 qaeco_spatial "SELECT p.coll_rate AS coll_rate, p.hour AS hour, p.month AS month, ST_intersection(r.geom,p.geom) AS geom FROM (SELECT d.coll_rate, d.hour, d.month, g.geom FROM gis_victoria.vic_gda9455_admin_state_1kmgrid AS g, vline.egk_coll_rate_glm AS d WHERE g.id = d.id) AS p, (SELECT ST_Union(geom) AS geom FROM vline.vic_gda9455_rail_vline_speeds) AS r"')
)

