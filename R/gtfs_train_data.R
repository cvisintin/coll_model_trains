require(RPostgreSQL)
require(lubridate)

download.file("http://data.ptv.vic.gov.au/downloads/gtfs.zip","data/gtfs.zip")

unzip("data/gtfs.zip", exdir="data/unzip")

unzip("data/unzip/1/google_transit.zip", exdir="data/gtfs")

unlink("data/unzip", recursive = TRUE, force = TRUE)
unlink("data/gtfs.zip", force = TRUE)

drv <- dbDriver("PostgreSQL")  #Specify a driver for postgreSQL type database
con <- dbConnect(drv, dbname="qaeco_spatial", user="qaeco", password="Qpostgres15", host="boab.qaeco.com", port="5432")  #Connection to database server on Boab

dbWriteTable(con, c("vline","calendar"), value=read.csv("data/gtfs/calendar.txt", header=TRUE, sep=","),overwrite=TRUE,row.names=FALSE)
dbWriteTable(con, c("vline","shapes_pts"), value=read.csv("data/gtfs/shapes.txt", header=TRUE, sep=","),overwrite=TRUE,row.names=FALSE)
dbGetQuery(con, "ALTER TABLE vline.shapes_pts ADD COLUMN id SERIAL")
dbGetQuery(con, "UPDATE vline.shapes_pts SET id = DEFAULT")
dbGetQuery(con, "ALTER TABLE vline.shapes_pts ADD PRIMARY KEY (id)")
dbWriteTable(con, c("vline","stop_times"), value=read.csv("data/gtfs/stop_times.txt", header=TRUE, sep=","),overwrite=TRUE,row.names=FALSE)
dbWriteTable(con, c("vline","stops"), value=read.csv("data/gtfs/stops.txt", header=TRUE, sep=","),overwrite=TRUE,row.names=FALSE)
dbWriteTable(con, c("vline","trips"), value=read.csv("data/gtfs/trips.txt", header=TRUE, sep=","),overwrite=TRUE,row.names=FALSE)

dbDisconnect(con)  #Disconnect database
dbUnloadDriver(drv)  #Remove driver

zip(paste("data/gtfs_",today(),".zip",sep=""),"data/gtfs/")
unlink("data/gtfs", recursive = TRUE, force = TRUE)
