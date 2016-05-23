library("RPostgreSQL")
library("lubridate")

download.file("http://data.ptv.vic.gov.au/downloads/gtfs.zip","gtfs.zip")

unzip("gtfs.zip", exdir="unzip")

setwd("./unzip/1/")
unzip("google_transit.zip", exdir="../../gtfs")

setwd("../../")

unlink("unzip", recursive = TRUE, force = TRUE)
unlink("gtfs.zip", force = TRUE)

drv <- dbDriver("PostgreSQL")  #Specify a driver for postgreSQL type database
con <- dbConnect(drv, dbname="qaeco_spatial", user="qaeco", password="Qpostgres15", host="boab.qaeco.com", port="5432")  #Connection to database server on Boab

summary(con)  #Tell us about connection
dbListTables(con)  #List tables in database

dbWriteTable(con, c("vline","calendar"), value=read.csv("gtfs/calendar.txt", header=TRUE, sep=","),overwrite=TRUE,row.names=FALSE)
dbWriteTable(con, c("vline","shapes_pts"), value=read.csv("gtfs/shapes.txt", header=TRUE, sep=","),overwrite=TRUE,row.names=FALSE)
dbGetQuery(con, "ALTER TABLE vline.shapes_pts ADD COLUMN id SERIAL")
dbGetQuery(con, "UPDATE vline.shapes_pts SET id = DEFAULT")
dbGetQuery(con, "ALTER TABLE vline.shapes_pts ADD PRIMARY KEY (id)")
dbWriteTable(con, c("vline","stop_times"), value=read.csv("gtfs/stop_times.txt", header=TRUE, sep=","),overwrite=TRUE,row.names=FALSE)
dbWriteTable(con, c("vline","stops"), value=read.csv("gtfs/stops.txt", header=TRUE, sep=","),overwrite=TRUE,row.names=FALSE)
dbWriteTable(con, c("vline","trips"), value=read.csv("gtfs/trips.txt", header=TRUE, sep=","),overwrite=TRUE,row.names=FALSE)

dbDisconnect(con)  #Disconnect database
dbUnloadDriver(drv)  #Remove driver

zip(paste("gtfs_",today(),".zip",sep=""),"gtfs/")
unlink("gtfs", recursive = TRUE, force = TRUE)
