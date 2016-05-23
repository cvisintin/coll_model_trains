library("RPostgreSQL")
library("rgdal")
library("rgeos")
library("maptools")
library("data.table")
library("zoo")
library("doMC")
library("lubridate")
library("stringr")
library("sp")

StrToSec <- function(x)
{
  x <- strsplit(x, ":")
  sapply(x, function(y) sum(as.numeric(y) * c(3600, 60, 1)))
}

mround <- function(x,base){
  base*round(x/base)
}

##### Read in data from PostGIS #####

drv <- dbDriver("PostgreSQL")  #specify a driver for postgreSQL type database
con <- dbConnect(drv, dbname="qaeco_spatial", user="qaeco", password="Qpostgres15", host="boab.qaeco.com", port="5432")  #define connection to database server on Boab

train_stops <- as.data.table(dbGetQuery(con,"SELECT stop_id, stop_lat, stop_lon FROM vline.stops;")) #query train stops as data.table

train_pts <- as.data.table(dbGetQuery(con,"SELECT id, shape_id, shape_pt_lat, shape_pt_lon, shape_dist_traveled as shape_dist FROM vline.shapes_pts;")) #query all spatial points (nodes of segments) of vline network
setkey(train_pts, id, shape_id, shape_dist) #define keys for data.table operations

#write.csv(train_pts, file = "train_pts.csv", row.names=FALSE)

# SELECT 
# stop_times.trip_id,
# stops.stop_lat, 
# stops.stop_lon, 
# stop_times.arrival_time,
# stop_times.stop_sequence,
# stop_times.shape_dist_traveled AS shape_dist, 
# calendar.monday, 
# calendar.tuesday, 
# calendar.wednesday, 
# calendar.thursday, 
# calendar.friday, 
# calendar.saturday, 
# calendar.sunday
# FROM 
# vline.calendar, 
# vline.stops, 
# vline.stop_times, 
# vline.trips
# WHERE 
# stop_times.trip_id = trips.trip_id AND
# stop_times.stop_id = stops.stop_id AND
# calendar.service_id = trips.service_id AND
# (calendar.monday != '0' OR
#  calendar.tuesday != '0' OR
#  calendar.wednesday != '0' OR
#  calendar.thursday != '0' OR
#  calendar.friday != '0' OR
#  calendar.saturday != '0' OR
#  calendar.sunday != '0')
# ORDER BY
# trip_id, stop_sequence;

train_rts <- as.data.table( #query all unique vline routes from schedule information in database
  dbGetQuery(con,"
             SELECT 
             stop_times_a.trip_id,
             stops.stop_lat, 
             stops.stop_lon, 
             stop_times_a.arrival_time,
             stop_times_a.stop_sequence,
             stop_times_a.shape_dist_traveled AS shape_dist, 
             calendar.monday, 
             calendar.tuesday, 
             calendar.wednesday, 
             calendar.thursday, 
             calendar.friday, 
             calendar.saturday, 
             calendar.sunday
             FROM 
             vline.calendar, 
             vline.stops, 
              (SELECT *
              FROM vline.stop_times
              WHERE
              shape_dist_traveled IS NOT NULL) stop_times_a, 
             vline.trips
             WHERE 
             stop_times_a.trip_id = trips.trip_id AND
             stop_times_a.stop_id = stops.stop_id AND
             calendar.service_id = trips.service_id AND
             (calendar.monday != '0' OR
              calendar.tuesday != '0' OR
              calendar.wednesday != '0' OR
              calendar.thursday != '0' OR
              calendar.friday != '0' OR
              calendar.saturday != '0' OR
              calendar.sunday != '0')
             ORDER BY
             trip_id, stop_sequence;
             ")
)
setkey(train_rts, trip_id, shape_dist) #define keys for data.table operations
setorder(train_rts, trip_id, stop_sequence) #sort and order data.table based on trip id and stop sequence

train_rts$shape_id <- str_sub(train_rts$trip_id,start=regexpr(".1", train_rts$trip_id, fixed = TRUE)+1) #split trip id sting to extract route information for matching to spatial points

#write.csv(train_rts, file = "train_rts.csv", row.names=FALSE)

train_rts_list <- unique(train_rts$trip_id) #create vector of unique trip ids


# registerDoMC(detectCores() - 1)
# 
# #i <- 883
# 
# system.time(
#   final_pt_times.plus24 <- foreach(i = 1:length(train_rts_list), .combine = rbind) %dopar% { #create data for each train route (specific run at scheduled time)
#     route.times <- merge(train_pts[shape_id==str_sub(paste(train_rts_list[i]),start=regexpr(".1", paste(train_rts_list[i]), fixed = TRUE)+1),],train_rts[trip_id==train_rts_list[i],],by=c("shape_id","shape_dist"),all.x=TRUE, allow.cartesian=TRUE) #combine spatial track points with timetable data based on matching trip id
#     route.dist <- route.times[,shape_dist] #create vector of distances along route for specific trip id
#     route.sched <- StrToSec(route.times[,arrival_time]) #convert arrival times to seconds from midnight (represented as 00:00:00 in data)
#     route.sched.final <- round(na.approx(route.sched, x=route.dist),0) #interpolate arrival times based on distance along route
#     route.times$arrival_time <- route.sched.final #replace arrival times with seconds in original data per route
# 
#     route.times2 <- route.times #copy data.table
#     hms <- seconds_to_period(route.times2[,arrival_time]) #convert seconds to days, hours, minutes, and seconds to account for +24 hour scheduling
#     hms2 <- sprintf('%02d:%02d:%02d', hms@hour, minute(hms), second(hms)) #convert to character vector of times to resolve all +24 hour to 00,01,02,etc...
#     route.times2[,arrival_time:=hms2] #replace arrival times with resolved times
#     
#     route.times2$arrival_time2 <- as.ITime(strptime(paste(substr(route.times2$arrival_time,1,5),":00",sep=""), format="%H:%M:%S")) #create new time column based on resolved times
#     sorted.data <- route.times2[order(route.times2$arrival_time),] #reorder data based on interpolated and resolved times
#     data.length <- length(sorted.data$arrival_time2) #calculate number of spatial points along route
#     time.min <- sorted.data$arrival_time2[1] #extract first time point in route
#     time.max <- sorted.data$arrival_time2[data.length] #extract last time point in route
#     all.dates <- seq(time.min, time.max, by=60) #create vector of 60 second (minute) intevals between first and last time points
#     all.dates.frame <- data.frame(list(arrival_time2=all.dates)) #covert to data.frame
#     merged.data <- merge(sorted.data, all.dates.frame, by="arrival_time2", all=TRUE) #merge data.frames to ensure there is a record for every minute in route
#     merged.data[is.na(merged.data[,arrival_time]), arrival_time := as.character(format(arrival_time2, format="%H:%M:%S"))] #update arrival times with interpolated missing values
#     merged.data[ ,shape_pt_lon := na.approx(shape_pt_lon, as.ITime(strptime(arrival_time, format="%H:%M:%S")))] #interpolate longitudes based on arrival times
#     merged.data[ ,shape_pt_lat := na.approx(shape_pt_lat, as.ITime(strptime(arrival_time, format="%H:%M:%S")))] #interpolate latitudes based on arrival times
#     merged.data[,c("shape_id","trip_id","monday","tuesday","wednesday","thursday","friday","saturday","sunday"):=list(shape_id[1],trip_id[1],monday[1],tuesday[1],wednesday[1],thursday[1],friday[1],saturday[1],sunday[1])] #replace NAs with contants for route
#     unique(merged.data[,.(trip_id,shape_id,shape_pt_lat,shape_pt_lon,arrival_time,monday,tuesday,wednesday,thursday,friday,saturday,sunday)]) #remove any duplicate records
#   }
# )
# 
# #final_pt_times.plus24[order(arrival_time),arrival_time,by=str_sub(arrival_time,1,2)]
# 
# #write.csv(final_pt_times.plus24, file = "final_pt_times_24h.csv", row.names=FALSE)
# 
# hour.seq <- format(seq(strptime("00:00:00", format="%H:%M:%S"), strptime("23:59:00", format="%H:%M:%S"), 3600), "%H:%M:%S")
# 
# trains.hour.plus24 <- data.table(HOUR=seq(0,23,1),TRAINS=rep(0,length(hour.seq)))
# for (i in 1:length(hour.seq)){
#   if (i < length(hour.seq)){
#     trains.hour.plus24[i,TRAINS:=nrow(final_pt_times.plus24[thursday==1 & arrival_time>=hour.seq[i] & arrival_time<hour.seq[i+1],.N, by=trip_id])]
#   }else{
#     trains.hour.plus24[i,TRAINS:=nrow(final_pt_times.plus24[thursday==1 & arrival_time>=hour.seq[i],.N, by=trip_id])]
#   }
# }
# 
# write.csv(trains.hour.plus24, file = "trains_hour.csv", row.names=FALSE)
# 
# train_rts$shape_id <- substr(train_rts$trip_id,13,35)
# 
# out.of.bounds <- train_rts[substr(arrival_time,1,2)>=24,.N,by=trip_id]
# 
# write.csv(out.of.bounds, file = "rts_over_24hr.csv", row.names=FALSE)

# registerDoMC(detectCores() - 1)
# 
# #i <- 883
# 
# system.time(
#   final_pt_times <- foreach(i = 1:length(train_rts_list), .combine = rbind) %dopar% {
#     route.times <- merge(train_pts[shape_id==str_sub(paste(train_rts_list[i]),start=regexpr(".1", paste(train_rts_list[i]), fixed = TRUE)+1),],train_rts[trip_id==train_rts_list[i],],by=c("shape_id","shape_dist"),all.x=TRUE, allow.cartesian=TRUE)
#     route.dist <- route.times[,shape_dist]
#     route.sched <- StrToSec(route.times[,arrival_time])
#     route.sched.final <- round(na.approx(route.sched, x=route.dist),0)
#     route.times$arrival_time <- route.sched.final
#   
#     if (min(route.times$arrival_time)<86400){
#       route.times2 <- route.times[arrival_time<86400,]
#       hms <- seconds_to_period(route.times2[,arrival_time])
#       hms2 <- sprintf('%02d:%02d:%02d', hms@hour, minute(hms), second(hms))
#       route.times2[,arrival_time:=hms2]
#       route.times2$arrival_time2 <- as.ITime(strptime(paste(substr(route.times2$arrival_time,1,5),":00",sep=""), format="%H:%M:%S"))
#       sorted.data <- route.times2[order(route.times2$arrival_time),]
#       data.length <- length(sorted.data$arrival_time2)
#       time.min <- sorted.data$arrival_time2[1]
#       time.max <- sorted.data$arrival_time2[data.length]
#       all.dates <- seq(time.min, time.max, by=60)
#       all.dates.frame <- data.frame(list(arrival_time2=all.dates))
#       merged.data <- merge(sorted.data, all.dates.frame, by="arrival_time2", all=TRUE)
#       merged.data[is.na(merged.data[,arrival_time]), arrival_time := as.character(format(arrival_time2, format="%H:%M:%S"))]
#       merged.data[ ,shape_pt_lon := na.approx(shape_pt_lon, as.ITime(strptime(arrival_time, format="%H:%M:%S")))]
#       merged.data[ ,shape_pt_lat := na.approx(shape_pt_lat, as.ITime(strptime(arrival_time, format="%H:%M:%S")))]
#       merged.data[,c("shape_id","trip_id","monday","tuesday","wednesday","thursday","friday","saturday","sunday"):=list(shape_id[1],trip_id[1],monday[1],tuesday[1],wednesday[1],thursday[1],friday[1],saturday[1],sunday[1])]
#       unique(merged.data[,.(trip_id,shape_id,shape_pt_lat,shape_pt_lon,arrival_time,monday,tuesday,wednesday,thursday,friday,saturday,sunday)])
#     }else{
#     }
#   }
# )
# 
# write.csv(final_pt_times, file = "final_pt_times.csv", row.names=FALSE)

# dbDisconnect(con)  #Disconnect database
# dbUnloadDriver(drv)  #Remove driver
# 
# #final_pt_times <- as.data.table(read.csv(file = "final_pt_times.csv", sep=",", stringsAsFactors=FALSE))
# 
# animate.seq <- format(seq(strptime("00:00:00", format="%H:%M:%S"), strptime("23:59:00", format="%H:%M:%S"), 60), "%H:%M:%S")
# 
# animate.seq.hr <- format(seq(strptime("00:00:00", format="%H:%M:%S"), strptime("23:59:00", format="%H:%M:%S"), 3600), "%H:%M:%S")
# 
# #i <- 370
# 
# vic.state <- readShapePoly("/home/casey/Research/GIS_Repo/VICTORIA/VIC_GDA94VICGRID_ADMIN_STATE.shp")
# train.routes <- readShapeLines("/home/casey/Research/GIS_Repo/VICTORIA/VIC_GDA94VICGRID_RAIL_VLINE_ROUTES.shp")
# vic.grid <- readShapePoly("/home/casey/Research/GIS_Repo/VICTORIA/VIC_GDA9455_ADMIN_STATE_1KMGRID.shp")
# 
# registerDoMC(detectCores() - 1)
# 
# non.null.plots <- foreach(i = 1:length(animate.seq), .packages = c("rgdal","rgeos","maptools","data.table"), .combine = c) %dopar% {
#   train.data <- final_pt_times.gis[thursday==1 & arrival_time>=animate.seq[i] & arrival_time<animate.seq[i+1],.(shape_id,shape_pt_lon,shape_pt_lat,arrival_time)]
#   train.coords <- data.frame(lon=train.data$shape_pt_lon, lat=train.data$shape_pt_lat)
#   if (nrow(train.coords) != 0){
#     print(i)
#   }
# }
# 
# #Create plots
# system.time(
#   final_plots <- foreach(i = non.null.plots, .packages = c("rgdal","rgeos","maptools","data.table"), .combine = c) %dopar% {
#     train.data <- final_pt_times[thursday==1 & arrival_time>=animate.seq[i] & arrival_time<animate.seq[i+1],.(shape_id,shape_pt_lon,shape_pt_lat,arrival_time)]
#     train.coords <- data.frame(lon=train.data$shape_pt_lon, lat=train.data$shape_pt_lat)
#     coordinates(train.coords) <- c("lon", "lat")
#     proj4string(train.coords) <- CRS("+init=epsg:4326") # WGS84
#     CRS.vicgrid <- CRS("+init=epsg:3111") # VicGrid94
#     train.vicgrid <- spTransform(train.coords, CRS.vicgrid)
#     png(paste("plots/train_plot_",which(non.null.plots == i),".png",sep=""), width=1753, height=1122, units="px")
#     par(mar = rep(0, 4))
#     plot(vic.state)
#     plot(train.routes, add = TRUE, col = "grey")
#     plot(train.vicgrid, add = TRUE, col = "red", pch = 16 )
#     text(2800000, 2300000, labels = animate.seq[i], cex=3)
#     dev.off()
#   }
# )
# 
# ##WORKING##
# final_pt_times.plots <- foreach(i = 1:length(train_rts_list), .combine = rbind) %dopar% { #create data for each train route (specific run at scheduled time)
#   rts <- train_rts[trip_id==train_rts_list[i],] #subset train route data
#   #rts$shape_dist2 <- mround(rts$shape_dist,5) #correct for mismatch due to change in data - rounds distance traveled to 5 metre intervals
#   pts <- unique(train_pts[shape_id==str_sub(paste(train_rts_list[i]),start=regexpr(".1", paste(train_rts_list[i]), fixed = TRUE)+1),.(shape_id,shape_pt_lat,shape_pt_lon,shape_dist)]) #subset spatial point data
#   #setnames(pts,"shape_pt_lat","stop_lat")
#   #setnames(pts,"shape_pt_lon","stop_lon")
#   #pts$shape_dist2 <- mround(pts$shape_dist,5) #correct for mismatch due to change in data - rounds distance traveled to 5 metre intervals 
#   route.times <- merge(pts,rts,by=c("shape_dist","shape_id"),all.x=TRUE, allow.cartesian=TRUE) #combine spatial track points with timetable data based on matching trip id
#   route.dist <- route.times[,shape_dist] #create vector of distances along route for specific trip id
#   route.sched <- StrToSec(route.times[,arrival_time]) #convert arrival times to seconds from midnight (represented as 00:00:00 in data)
#   route.sched.final <- round(na.approx(route.sched, x=route.dist),0) #interpolate arrival times based on distance along route - assumes constant velocity of trains which is not realistic but approximately useful for now
#   route.times$arrival_time <- route.sched.final #replace arrival times with interpolated vector of values
#   hms <- seconds_to_period(route.times[,arrival_time]) #convert seconds to days, hours, minutes, and seconds to account for +24 hour scheduling
#   hms2 <- sprintf('%02d:%02d:%02d', hms@hour, minute(hms), second(hms)) #convert to character vector of times to resolve all +24 hour to 00,01,02,etc...
#   route.times$arrival_time <- hms2 #replace arrival times with seconds in original data per route  
#   
#   route.times$arrival_time2 <- StrToSec(route.times[,arrival_time]) #create new time column based on resolved times
#   sorted.data <- route.times[order(route.times2$arrival_time),] #reorder data based on interpolated and resolved times
#   data.length <- length(sorted.data$arrival_time2) #calculate number of spatial points along route
#   time.min <- sorted.data$arrival_time2[1] #extract first time point in route
#   time.max <- sorted.data$arrival_time2[data.length] #extract last time point in route
#   all.dates <- seq(time.min, time.max, by=60) #create vector of 60 second (minute) intevals between first and last time points
#   hms <- seconds_to_period(all.dates)
#   hms2 <- sprintf('%02d:%02d:%02d', hms@hour, minute(hms), second(hms))
#   all.dates.frame <- data.frame(list(arrival_time=hms2)) #covert to data.frame
#   merged.data <- merge(sorted.data, all.dates.frame, by="arrival_time", all=TRUE) #merge data.frames to ensure there is a record for every minute in route
#   merged.data[is.na(merged.data[,arrival_time]), arrival_time := as.character(format(arrival_time2, format="%H:%M:%S"))] #update arrival times with interpolated missing values
#   merged.data[ ,shape_pt_lon := na.approx(shape_pt_lon, as.ITime(strptime(arrival_time, format="%H:%M:%S")))] #interpolate longitudes based on arrival times
#   merged.data[ ,shape_pt_lat := na.approx(shape_pt_lat, as.ITime(strptime(arrival_time, format="%H:%M:%S")))] #interpolate latitudes based on arrival times
#   merged.data[,c("shape_id","trip_id","monday","tuesday","wednesday","thursday","friday","saturday","sunday"):=list(shape_id[1],trip_id[1],monday[1],tuesday[1],wednesday[1],thursday[1],friday[1],saturday[1],sunday[1])] #replace NAs with contants for route
#   unique(merged.data[,.(trip_id,shape_id,shape_dist,shape_pt_lat,shape_pt_lon,arrival_time,monday,tuesday,wednesday,thursday,friday,saturday,sunday)]) #remove any duplicate records
#   
# 
#   #route.times[,c("shape_id","trip_id","monday","tuesday","wednesday","thursday","friday","saturday","sunday"):=list(shape_id[1],trip_id[1],monday[1],tuesday[1],wednesday[1],thursday[1],friday[1],saturday[1],sunday[1])] #replace NAs with constants for route
#   #unique(route.times[,.(trip_id,shape_id,shape_dist,shape_pt_lat,shape_pt_lon,arrival_time,monday,tuesday,wednesday,thursday,friday,saturday,sunday)]) #remove any duplicate records
# }



##### Create spatially & temporally-referenced dataset #####

registerDoMC(detectCores() - 1) #initialise processor cores for parallel operations leaving one free for the operating system

final_pt_times.gis <- foreach(i = 1:length(train_rts_list), .combine = rbind) %:%   when(train_rts[trip_id==train_rts_list[i],.N]>1) %dopar% { #create data for each train route (specific run at scheduled time)
  rts <- train_rts[trip_id==train_rts_list[i],] #subset train route data
  pts <- unique(train_pts[shape_id==str_sub(paste(train_rts_list[i]),start=regexpr(".1", paste(train_rts_list[i]), fixed = TRUE)+1),.(shape_id,shape_pt_lat,shape_pt_lon,shape_dist)]) #subset spatial point data
  route.times <- merge(pts,rts,by=c("shape_dist","shape_id"),all.x=TRUE, allow.cartesian=TRUE) #combine spatial track points with timetable data based on matching trip id
  route.dist <- route.times[,shape_dist] #create vector of distances along route for specific trip id
  route.sched <- StrToSec(route.times[,arrival_time]) #convert arrival times to seconds from midnight (represented as 00:00:00 in data)
  route.sched.final <- round(na.approx(route.sched, x=route.dist),0) #interpolate arrival times based on distance along route - assumes constant velocity of trains which is not realistic but approximately useful for now
  route.times$arrival_time <- route.sched.final #replace arrival times with interpolated vector of values
  hms <- seconds_to_period(route.times[,arrival_time]) #convert seconds to days, hours, minutes, and seconds to account for +24 hour scheduling
  hms2 <- sprintf('%02d:%02d:%02d', hms@hour, minute(hms), second(hms)) #convert to character vector of times to resolve all +24 hour to 00,01,02,etc...
  route.times$arrival_time <- hms2 #replace arrival times with seconds in original data per route
  coords <- route.times[,.("lon"=shape_pt_lon,"lat"=shape_pt_lat)]
  coordinates(coords) <- c("lon", "lat")
  proj4string(coords) <- CRS("+init=epsg:4326") # WGS84
  route.times[,x:=spTransform(coords, CRS("+init=epsg:28355"))$lon] # MGA9455
  route.times[,y:=spTransform(coords, CRS("+init=epsg:28355"))$lat] # MGA9455
  route.times[,c("shape_id","trip_id","monday","tuesday","wednesday","thursday","friday","saturday","sunday"):=list(shape_id[1],trip_id[1],monday[1],tuesday[1],wednesday[1],thursday[1],friday[1],saturday[1],sunday[1])] #replace NAs with constants for route
  unique(route.times[,.(trip_id,shape_id,shape_dist,shape_pt_lon,shape_pt_lat,x,y,arrival_time,monday,tuesday,wednesday,thursday,friday,saturday,sunday)]) #remove any duplicate records
}

#dow <- "thursday"
dow <- c("sunday","monday","tuesday","wednesday","thursday","friday","saturday") #create vector of days of the week for loops

##### Create verification plots #####

vic.state <- readShapePoly("/home/casey/Research/GIS_Repo/VICTORIA/VIC_GDA94VICGRID_ADMIN_STATE.shp") #load in state boundary GIS
train.routes <- readShapeLines("/home/casey/Research/GIS_Repo/VICTORIA/VIC_GDA94VICGRID_RAIL_VLINE_ROUTES.shp") #load vline rail network GIS

animate.seq <- format(seq(strptime("00:00:00", format="%H:%M:%S"), strptime("23:59:00", format="%H:%M:%S"), 60), "%H:%M:%S") #create sequence for animation at 60 second intervals

non.null.plots <- foreach(i = 1:length(animate.seq), .packages = c("rgdal","rgeos","maptools","data.table"), .combine = c) %dopar% { #determine which plots (times) have trains on network
  train.data <- final_pt_times.gis[thursday==1 & arrival_time>=animate.seq[i] & arrival_time<animate.seq[i+1],.(shape_id,shape_pt_lon,shape_pt_lat,arrival_time)] #gather all points within time sequence
  if (nrow(train.data) != 0){ #verify frame has at least one train
    print(i) #record plot number
  }
}

final_plots <- foreach(i = non.null.plots, .packages = c("rgdal","rgeos","maptools","data.table"), .combine = c) %dopar% { #produce plots where trains are present
  train.data <- final_pt_times.gis[thursday==1 & arrival_time>=animate.seq[i] & arrival_time<animate.seq[i+1],.(shape_id,shape_pt_lon,shape_pt_lat,arrival_time)] #subset data based on time 
  train.coords <- data.frame(lon=train.data$shape_pt_lon, lat=train.data$shape_pt_lat) #extract coordinates
  coordinates(train.coords) <- c("lon", "lat") #create spatial object
  proj4string(train.coords) <- CRS("+init=epsg:4326") #define coordinate system as WGS84
  CRS.vicgrid <- CRS("+init=epsg:3111") #define VicGrid94 projection
  train.vicgrid <- spTransform(train.coords, CRS.vicgrid) #reproject coordinates to VicGrid94
  png(paste("plots/thur_train_plot_",which(non.null.plots == i),".png",sep=""), width=1753, height=1122, units="px") #create png plotting device
  par(mar = rep(0, 4)) #remove margins
  plot(vic.state) #plot the state boundaries
  plot(train.routes, add = TRUE, col = "grey") #add the vline rail network
  plot(train.vicgrid, add = TRUE, col = "red", pch = 16 ) #add train locations
  text(2800000, 2300000, labels = animate.seq[i], cex=3) #add a timestamp to the plot
  dev.off() #write png file
}



#Create Point Shapefiles
# system.time(
#   for(i in non.null.trains.hr){
#     if (i < length(non.null.trains.hr)){
#       SPDF <- SpatialPointsDataFrame(data.frame(lon=0, lat=0), data.frame(TRAIN=NA), proj4string = CRS("+init=epsg:28355"))
#       for(j in final_pt_times.gis[thursday==1 & arrival_time>=animate.seq.hr[i] & arrival_time<animate.seq.hr[i+1],unique(trip_id)]){
#         train.data <- final_pt_times.gis[thursday==1 & arrival_time>=animate.seq.hr[i] & arrival_time<animate.seq.hr[i+1] & trip_id==paste(j),.(shape_id,shape_pt_lon,shape_pt_lat,arrival_time)]
#         train.coords <- data.frame(lon=train.data$shape_pt_lon, lat=train.data$shape_pt_lat)
#         coordinates(train.coords) <- c("lon", "lat")
#         proj4string(train.coords) <- CRS("+init=epsg:4326") # WGS84
#         CRS.mga9455 <- CRS("+init=epsg:28355") # MGA9455
#         train.mga9455 <- spTransform(train.coords, CRS.mga9455)
#         tempSPDF <- SpatialPointsDataFrame(coords=train.mga9455, data=data.frame(TRAIN=rep(paste(j),length(train.mga9455))))
#         SPDF <- spRbind(SPDF,tempSPDF)
#       }
#       SPDF <- SPDF[-1,]
#       writeOGR(SPDF, ".", paste("shp_files/VIC_GDA9455_TRAINS_POINTS_",str_sub(animate.seq.hr[i],1,2),sep=""), driver="ESRI Shapefile")
#     }else{
#       SPDF <- SpatialPointsDataFrame(data.frame(lon=0, lat=0), data.frame(TRAIN=NA), proj4string = CRS("+init=epsg:28355"))
#       for(j in final_pt_times.gis[thursday==1 & arrival_time>=animate.seq.hr[i],unique(trip_id)]){
#         train.data <- final_pt_times.gis[thursday==1 & arrival_time>=animate.seq.hr[i] & trip_id==paste(j),.(shape_id,shape_pt_lon,shape_pt_lat,arrival_time)]
#         train.coords <- data.frame(lon=train.data$shape_pt_lon, lat=train.data$shape_pt_lat)
#         coordinates(train.coords) <- c("lon", "lat")
#         proj4string(train.coords) <- CRS("+init=epsg:4326") # WGS84
#         CRS.mga9455 <- CRS("+init=epsg:28355") # MGA9455
#         train.mga9455 <- spTransform(train.coords, CRS.mga9455)
#         tempSPDF <- SpatialPointsDataFrame(coords=train.mga9455, data=data.frame(TRAIN=rep(paste(j),length(train.mga9455))))
#         SPDF <- spRbind(SPDF,tempSPDF)
#       }
#       SPDF <- SPDF[-1,]
#       writeOGR(SPDF, ".", paste("shp_files/VIC_GDA9455_TRAINS_POINTS_",str_sub(animate.seq.hr[i],1,2),sep=""), driver="ESRI Shapefile")      
#     }
#   }
# )

##### Create polyline shapefiles #####

animate.seq.hr <- format(seq(strptime("00:00:00", format="%H:%M:%S"), strptime("23:59:00", format="%H:%M:%S"), 3600), "%H:%M:%S") #produce sequence of values at one hour intervals

for (i in dow){ #for each day of the week (train schedules vary)
  assign(paste0("train_hrs_",i), #write vector of hours for day of week
         foreach(j = 1:length(animate.seq.hr), .packages = c("rgdal","rgeos","maptools","data.table"), .combine = c) %dopar% { #determine if any hours have no trains on network
           if (j < length(animate.seq.hr)){ #each hour up to maximum for range
             train.data <- final_pt_times.gis[get(i)==1 & arrival_time>=animate.seq.hr[j] & arrival_time<animate.seq.hr[j+1],.(shape_id)] #gather all data within time sequence for day of week
             if (nrow(train.data) != 0){ #verify data has at least one train
               print(j) #record hour
             }
           }else{ #for final hour
             train.data <- final_pt_times.gis[get(i)==1 & arrival_time>=animate.seq.hr[j],.(shape_id)] #gather all data within time sequence
             if (nrow(train.data) != 0){ #verify data has at least one train
               print(j) #record hour
             }
           }
         }
  )
}

for(i in train_hrs_thursday){ #produce data where trains are present
  if (i < max(train_hrs_thursday)){ #each hour up to maximum for range
    SPLDF <- foreach(j = final_pt_times.gis[thursday==1 & arrival_time>=animate.seq.hr[i] & arrival_time<animate.seq.hr[i+1],unique(trip_id)], .packages = c("rgdal","rgeos","maptools","data.table"), .combine = spRbind) %dopar% {
      train.data <- final_pt_times.gis[thursday==1 & arrival_time>=animate.seq.hr[i] & arrival_time<animate.seq.hr[i+1] & trip_id==paste(j),.(shape_id,shape_dist,shape_pt_lon,shape_pt_lat,arrival_time)]
      train.coords <- data.frame(lon=train.data$shape_pt_lon, lat=train.data$shape_pt_lat) #extract coordinates
      coordinates(train.coords) <- c("lon", "lat") #create spatial object
      proj4string(train.coords) <- CRS("+init=epsg:4326") #define coordinate system as WGS84
      CRS.mga9455 <- CRS("+init=epsg:28355") #define GDA94MGA55 projection
      train.mga9455 <- spTransform(train.coords, CRS.mga9455) #reproject coordinates to GDA94MGA55
      lines <- SpatialLines(list(Lines(list(Line(train.mga9455)), paste(j))), proj4string = CRS("+init=epsg:28355")) #create polyline by connecting all coordinates on trip
      data <- data.frame(TRAIN=rep(paste(j),length(lines))) #create data.frame with trip id repeated for each individual polyline
      rownames(data) <- data$TRAIN #replace row.names with trip id
      SPLDF <- SpatialLinesDataFrame(lines, data) #combine polylines and data
      SPLDF #add spatiallinedataframe to foreach object
    }
    writeOGR(SPLDF, ".", paste("shp_files/VIC_GDA9455_TRAINS_PLINES_THUR_",str_sub(animate.seq.hr[i],1,2),sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE) #write aggregated foreach object (multiple polyline data combined by hour) to shapefile
  }else{ #for final hour
    SPLDF <- foreach(j = final_pt_times.gis[thursday==1 & arrival_time>=animate.seq.hr[i],unique(trip_id)], .packages = c("rgdal","rgeos","maptools","data.table"), .combine = spRbind) %dopar% {
      train.data <- final_pt_times.gis[thursday==1 & arrival_time>=animate.seq.hr[i] & trip_id==paste(j),.(shape_id,shape_pt_lon,shape_pt_lat,arrival_time)]
      train.coords <- data.frame(lon=train.data$shape_pt_lon, lat=train.data$shape_pt_lat) #extract coordinates
      coordinates(train.coords) <- c("lon", "lat") #create spatial object
      proj4string(train.coords) <- CRS("+init=epsg:4326") #define coordinate system as WGS84
      CRS.mga9455 <- CRS("+init=epsg:28355") #define GDA94MGA55 projection
      train.mga9455 <- spTransform(train.coords, CRS.mga9455) #reproject coordinates to GDA94MGA55
      lines <- SpatialLines(list(Lines(list(Line(train.mga9455)), paste(j))), proj4string = CRS("+init=epsg:28355")) #create polyline by connecting all coordinates on trip
      data <- data.frame(TRAIN=rep(paste(j),length(lines))) #create data.frame with trip id repeated for each individual polyline
      rownames(data) <- data$TRAIN #replace row.names with trip id
      SPLDF <- SpatialLinesDataFrame(lines, data) #combine polylines and data
      SPLDF #add spatiallinedataframe to foreach object
    }
    writeOGR(SPLDF, ".", paste("shp_files/VIC_GDA9455_TRAINS_PLINES_THUR_",str_sub(animate.seq.hr[i],1,2),sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE) #write aggregated foreach object (multiple polyline data combined by hour) to shapefile     
  }
}

##### Create train speed shapefiles #####

# for (i in dow) { #for each day of the week (train schedules vary)
#   for(j in get(paste0("train_hrs_",i))){
#     if (j < max(get(paste0("train_hrs_",i)))){
#       SPLDF <- foreach(k = final_pt_times.gis[get(i)==1 & arrival_time>=animate.seq.hr[j] & arrival_time<animate.seq.hr[j+1],unique(trip_id)], .packages = c("rgdal","rgeos","maptools","data.table"), .combine = spRbind) %:% when(nrow(final_pt_times.gis[get(i)==1 & arrival_time>=animate.seq.hr[j] & arrival_time<animate.seq.hr[j+1] & trip_id==paste(k),.(shape_id,shape_dist,shape_pt_lon,shape_pt_lat,arrival_time)])!=1) %dopar% {
#         train.data <- final_pt_times.gis[get(i)==1 & arrival_time>=animate.seq.hr[j] & arrival_time<animate.seq.hr[j+1] & trip_id==paste(k),.(shape_id,shape_dist,shape_pt_lon,shape_pt_lat,arrival_time)]
#         train.coords <- data.frame(lon=train.data$shape_pt_lon, lat=train.data$shape_pt_lat)
#         coordinates(train.coords) <- c("lon", "lat")
#         proj4string(train.coords) <- CRS("+init=epsg:4326") # WGS84
#         CRS.mga9455 <- CRS("+init=epsg:28355") # MGA9455
#         train.mga9455 <- spTransform(train.coords, CRS.mga9455)
#         kph <- NULL
#         SPL <- NULL
#         for (m in 2:nrow(train.mga9455@coords)){
#           km <- sqrt(sum((train.mga9455@coords[m,] - train.mga9455@coords[m-1,]) ^ 2))/1000
#           hour <- abs(StrToSec(train.data$arrival_time[m])-StrToSec(train.data$arrival_time[m-1]))/3600
#           kph[m-1] <- km/hour
#           if (m==2){
#             SPL <- list(Lines(list(Line(train.mga9455@coords[c(m-1,m),])),paste0(k,"-",m-1)))
#           }else{
#             SPL[m-1] <- list(Lines(list(Line(train.mga9455@coords[c(m-1,m),])),paste0(k,"-",m-1)))
#           }
#         }
#         lines <- SpatialLines(SPL, proj4string = CRS("+init=epsg:28355"))
#         data <- data.frame(train=rep(paste(k),length(lines)),speed=kph,hour=rep(as.integer(str_sub(animate.seq.hr[j],1,2)),length(lines)),dow=rep(paste(i),length(lines)))
#         row.names(data) <- paste0(rep(k,length(row.names(data))),"-",row.names(data))
#         SPLDF <- SpatialLinesDataFrame(lines, data)
#         SPLDF
#       }
#       writeOGR(SPLDF, ".", paste("shp_files/train_speeds_",i,"_",str_sub(animate.seq.hr[j],1,2),sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
#     }else{
#       SPLDF <- foreach(k = final_pt_times.gis[get(i)==1 & arrival_time>=animate.seq.hr[j],unique(trip_id)], .packages = c("rgdal","rgeos","maptools","data.table"), .combine = spRbind) %:% when(nrow(final_pt_times.gis[get(i)==1 & arrival_time>=animate.seq.hr[j] & trip_id==paste(k),.(shape_id,shape_pt_lon,shape_pt_lat,arrival_time)])!=1) %dopar% {
#         train.data <- final_pt_times.gis[get(i)==1 & arrival_time>=animate.seq.hr[j] & trip_id==paste(k),.(shape_id,shape_pt_lon,shape_pt_lat,arrival_time)]
#         train.coords <- data.frame(lon=train.data$shape_pt_lon, lat=train.data$shape_pt_lat)
#         coordinates(train.coords) <- c("lon", "lat")
#         proj4string(train.coords) <- CRS("+init=epsg:4326") # WGS84
#         CRS.mga9455 <- CRS("+init=epsg:28355") # MGA9455
#         train.mga9455 <- spTransform(train.coords, CRS.mga9455)
#         kph <- NULL
#         SPL <- NULL
#         for (m in 2:nrow(train.mga9455@coords)){
#           km <- sqrt(sum((train.mga9455@coords[m,] - train.mga9455@coords[m-1,]) ^ 2))/1000
#           hour <- abs(StrToSec(train.data$arrival_time[m])-StrToSec(train.data$arrival_time[m-1]))/3600
#           kph[m-1] <- km/hour
#           if (m==2){
#             SPL <- list(Lines(list(Line(train.mga9455@coords[c(m-1,m),])),paste0(k,"-",m-1)))
#           }else{
#             SPL[m-1] <- list(Lines(list(Line(train.mga9455@coords[c(m-1,m),])),paste0(k,"-",m-1)))
#           }
#         }
#         lines <- SpatialLines(SPL, proj4string = CRS("+init=epsg:28355"))
#         data <- data.frame(train=rep(paste(k),length(lines)),speed=kph,hour=rep(as.integer(str_sub(animate.seq.hr[j],1,2)),length(lines)),dow=rep(paste(i),length(lines)))
#         row.names(data) <- paste0(rep(k,length(row.names(data))),"-",row.names(data))
#         SPLDF <- SpatialLinesDataFrame(lines, data)
#         SPLDF
#       }
#       writeOGR(SPLDF, ".", paste("shp_files/train_speeds_",i,"_",str_sub(animate.seq.hr[j],1,2),sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)      
#     }
#   }
# }

SPLDF_TOT <- foreach(i = dow, .packages = c("rgdal","rgeos","maptools","data.table"), .combine=spRbind) %dopar% { #for each day of the week (train schedules vary)
  SPLDF_DAY <- vector(mode='list',length(get(paste0("train_hrs_",i))))
  for (j in get(paste0("train_hrs_",i))){
    if (j < max(get(paste0("train_hrs_",i)))){
      trips <- final_pt_times.gis[get(i)==1 & arrival_time>=animate.seq.hr[j] & arrival_time<animate.seq.hr[j+1],unique(trip_id)]
      SPLDF <- vector(mode='list',length(trips))
      for(k in trips){
        if(nrow(final_pt_times.gis[get(i)==1 & arrival_time>=animate.seq.hr[j] & arrival_time<animate.seq.hr[j+1] & trip_id==paste(k),.(shape_id)])<=1){
          next
        }
        train.data <- final_pt_times.gis[get(i)==1 & arrival_time>=animate.seq.hr[j] & arrival_time<animate.seq.hr[j+1] & trip_id==paste(k),.(shape_id,shape_dist,x,y,arrival_time)]
        kph <- as.vector(rep(0.0,nrow(train.data[,.(x,y)])-1))
        SPL <- vector(mode='list',nrow(train.data[,.(x,y)])-1)
        for (m in 2:nrow(train.data[,.(x,y)])){
          km <- sqrt(sum((train.data[m,.(x,y)] - train.data[m-1,.(x,y)]) ^ 2))/1000
          hour <- abs(StrToSec(train.data$arrival_time[m])-StrToSec(train.data$arrival_time[m-1]))/3600
          kph[m-1] <- km/hour
          if (m==2){
            SPL <- list(Lines(list(Line(train.data[c(m-1,m),.(x,y)])),paste0(k,"-",str_sub(dow[i],1,2),"-",j,"-",m-1)))
          }else{
            SPL[m-1] <- list(Lines(list(Line(train.data[c(m-1,m),.(x,y)])),paste0(k,"-",str_sub(dow[i],1,2),"-",j,"-",m-1)))
          }
        }
        lines <- SpatialLines(SPL, proj4string = CRS("+init=epsg:28355"))
        data <- data.frame("train"=rep(paste(k),length(lines)),"speed"=kph,"hour"=rep(as.integer(str_sub(animate.seq.hr[j],1,2)),length(lines)),"dow"=rep(paste(i),length(lines)))
        row.names(data) <- paste0(rep(k,length(row.names(data))),"-",rep(str_sub(dow[i],1,2),length(row.names(data))),"-",rep(j,length(row.names(data))),"-",row.names(data))
        SPLDF[which(trips==k)] <- SpatialLinesDataFrame(lines, data)
      }
      SPLDF <- SPLDF[!sapply(SPLDF, is.null)]
      #SPLDF2 <- do.call(rbind,SPLDF)
      SPLDF_DAY[which(get(paste0("train_hrs_",i))==j)] <- do.call(rbind,SPLDF)
      #writeOGR(SPLDF2, ".", paste("shp_files/train_speeds_",i,"_",str_sub(animate.seq.hr[j],1,2),sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
    
    }else{
      trips <- final_pt_times.gis[get(i)==1 & arrival_time>=animate.seq.hr[j],unique(trip_id)]
      SPLDF <- vector(mode='list',length(trips))
      for(k in trips){
        if(nrow(final_pt_times.gis[get(i)==1 & arrival_time>=animate.seq.hr[j] & trip_id==paste(k),.(shape_id)])<=1){
          next
        }
        train.data <- final_pt_times.gis[get(i)==1 & arrival_time>=animate.seq.hr[j] & trip_id==paste(k),.(shape_id,x,y,arrival_time)]
        kph <- as.vector(rep(0.0,nrow(train.data[,.(x,y)])-1))
        SPL <- vector(mode='list',nrow(train.data[,.(x,y)])-1)
        for (m in 2:nrow(train.data[,.(x,y)])){
          km <- sqrt(sum((train.data[m,.(x,y)] - train.data[m-1,.(x,y)]) ^ 2))/1000
          hour <- abs(StrToSec(train.data$arrival_time[m])-StrToSec(train.data$arrival_time[m-1]))/3600
          kph[m-1] <- km/hour
          if (m==2){
            SPL <- list(Lines(list(Line(train.data[c(m-1,m),.(x,y)])),paste0(k,"-",str_sub(dow[i],1,2),"-",j,"-",m-1)))
          }else{
            SPL[m-1] <- list(Lines(list(Line(train.data[c(m-1,m),.(x,y)])),paste0(k,"-",str_sub(dow[i],1,2),"-",j,"-",m-1)))
          }
        }
        lines <- SpatialLines(SPL, proj4string = CRS("+init=epsg:28355"))
        data <- data.frame(train=rep(paste(k),length(lines)),speed=kph,hour=rep(as.integer(str_sub(animate.seq.hr[j],1,2)),length(lines)),dow=rep(paste(i),length(lines)))
        row.names(data) <- paste0(rep(k,length(row.names(data))),"-",rep(str_sub(dow[i],1,2),length(row.names(data))),"-",rep(j,length(row.names(data))),"-",row.names(data))
        SPLDF[which(trips==k)] <- SpatialLinesDataFrame(lines, data)
      }
      SPLDF <- SPLDF[!sapply(SPLDF, is.null)]
      #SPLDF2 <- do.call(rbind,SPLDF)
      SPLDF_DAY[which(get(paste0("train_hrs_",i))==j)] <- do.call(rbind,SPLDF)
      #writeOGR(SPLDF2, ".", paste("shp_files/train_speeds_",i,"_",str_sub(animate.seq.hr[j],1,2),sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)      
    }
  }
  do.call(rbind,SPLDF_DAY)
}

##### Write spatial data to database #####

writeOGR(SPLDF_TOT, dsn="PG:dbname=qaeco_spatial user=qaeco password=Qpostgres15 host=boab.qaeco.com port=5432", layer="vline.vic_gda9455_rail_vline_speeds", driver="PostgreSQL", layer_options = "geometry_name=geom", overwrite_layer=TRUE)

#ALTER TABLE vline.vic_gda9455_rail_vline_speeds DROP COLUMN ogc_fid;
#ALTER TABLE vline.vic_gda9455_rail_vline_speeds ADD COLUMN id SERIAL PRIMARY KEY;

dbGetQuery(con,paste0("CREATE INDEX vic_gda9455_rail_vline_speeds_gix ON vline.vic_gda9455_rail_vline_speeds USING GIST (geom);"))
dbGetQuery(con,paste0("SELECT UpdateGeometrySRID('vline','vic_gda9455_rail_vline_speeds','geom',28355);"))
dbGetQuery(con,paste0("VACUUM ANALYZE vline.vic_gda9455_rail_vline_speeds;"))

#Train routes per hour and day
files <- list.files(pattern="VIC_GDA9455_TRAINS_PLINES_.*\\.shp", recursive=TRUE, full.names=TRUE)
for (i in 1:length(files)){
  shpfile <- readShapeLines(files[i])
  name <- unlist(strsplit(files[i],"\\.shp"))[(1:(2*(length(files[i])))*2)-1][1:length(files[i])]
  name <- str_sub(paste(name),start=regexpr("/VIC", paste(name), fixed = TRUE)+1)
  
  proj4string(shpfile) <- CRS("+init=epsg:28355")
  writeOGR(shpfile, dsn="PG:dbname=qaeco_spatial user=qaeco password=Qpostgres15 host=boab.qaeco.com port=5432", layer=paste0("vline.",tolower(name)), driver="PostgreSQL", layer_options = "geometry_name=geom", overwrite_layer=TRUE)
  #dbClearResult(dbListResults(con)[[1]])
  dbGetQuery(con,paste0("CREATE INDEX ",tolower(name),"_gix ON ",paste0("vline.",tolower(name))," USING GIST (geom);"))
  dbGetQuery(con,paste0("VACUUM ANALYZE ",paste0("vline.",tolower(name)),";"))
  dbGetQuery(con,paste0("SELECT UpdateGeometrySRID('vline','",tolower(name),"','geom',28355);"))
}

#Train speeds per hour and day by route
files <- list.files(pattern="VIC_GDA9455_TRAINS_SPEEDS_.*\\.shp", recursive=TRUE, full.names=TRUE)
for (i in 1:length(files)){
  shpfile <- readShapeLines(files[i])
  name <- unlist(strsplit(files[i],"\\.shp"))[(1:(2*(length(files[i])))*2)-1][1:length(files[i])]
  name <- str_sub(paste(name),start=regexpr("/VIC", paste(name), fixed = TRUE)+1)
  
  proj4string(shpfile) <- CRS("+init=epsg:28355")
  writeOGR(shpfile, dsn="PG:dbname=qaeco_spatial user=qaeco password=Qpostgres15 host=boab.qaeco.com port=5432", layer=paste0("vline.",tolower(name)), driver="PostgreSQL", layer_options = "geometry_name=geom", overwrite_layer=TRUE)
  #dbClearResult(dbListResults(con)[[1]])
  dbGetQuery(con,paste0("CREATE INDEX ",tolower(name),"_gix ON ",paste0("vline.",tolower(name))," USING GIST (geom);"))
  dbGetQuery(con,paste0("VACUUM ANALYZE ",paste0("vline.",tolower(name)),";"))
  dbGetQuery(con,paste0("SELECT UpdateGeometrySRID('vline','",tolower(name),"','geom',28355);"))
}

#Eastern grey kangaroo presence (correlative - values fixed across time)
grid <- readShapePoly("/home/casey/Research/GIS_Repo/VICTORIA/VIC_GDA9455_ADMIN_STATE_1KMGRID_EGK.shp")
proj4string(grid) <- CRS("+init=epsg:28355")
writeOGR(grid, dsn="PG:dbname=qaeco_spatial user=qaeco password=Qpostgres15 host=boab.qaeco.com port=5432", layer="gis_victoria.vic_gda9455_admin_state_1kmgrid", driver="PostgreSQL", layer_options = "geometry_name=geom")
dbGetQuery(con,paste0("SELECT UpdateGeometrySRID('gis_victoria','vic_gda9455_admin_state_1kmgrid','geom',28355);"))
dbGetQuery(con,paste0("CREATE INDEX vic_gda9455_admin_state_1kmgrid_gix ON gis_victoria.vic_gda9455_admin_state_1kmgrid USING GIST (geom);"))
dbGetQuery(con,paste0("VACUUM ANALYZE gis_victoria.vic_gda9455_admin_state_1kmgrid;"))

#rail_net <- readShapeLines("/home/casey/Research/GIS_Repo/VICTORIA/VIC_GDA9455_RAIL_VLINE_ROUTES.shp")
#proj4string(rail_net) <- CRS("+init=epsg:28355")
#writeOGR(rail_net, dsn="PG:dbname=qaeco_spatial user=qaeco password=Qpostgres15 host=boab.qaeco.com port=5432", layer="vline.vic_gda9455_rail_vline_routes", driver="PostgreSQL", layer_options = "geometry_name=geom")
#dbSendQuery(con,paste0("CREATE INDEX vic_gda9455_rail_vline_routes_gix ON vline.vic_gda9455_rail_vline_routes USING GIST (geom);"))
#dbSendQuery(con,paste0("VACUUM ANALYZE vline.vic_gda9455_rail_vline_routes;"))

##### Perform spatial operations in database #####

dbGetQuery(con,paste0("
  CREATE TABLE vline.vic_gda9455_rail_vline_routes (id serial, hour integer);
  SELECT AddGeometryColumn('vline', 'vic_gda9455_rail_vline_routes','geom',28355,'LINESTRING',2);
  "))
for(i in non.null.trains.hr){
  dbGetQuery(con,paste0("
    INSERT INTO
      vline.vic_gda9455_rail_vline_routes (hour, geom)
    SELECT
      ",as.integer(str_sub(animate.seq.hr[i],1,2))," AS hour, vic_gda9455_trains_plines_",str_sub(animate.seq.hr[i],1,2),".geom AS geom
    FROM
      vline.vic_gda9455_trains_plines_",str_sub(animate.seq.hr[i],1,2),";
  "))
}


dbGetQuery(con,paste0("
  CREATE TABLE vline.vic_gda9455_rail_vline_speeds (id serial, hour integer, train character varying, speed double precision);
  SELECT AddGeometryColumn('vline', 'vic_gda9455_rail_vline_speeds','geom',28355,'LINESTRING',2);
  "))
for(i in non.null.trains.hr){
  dbGetQuery(con,paste0("
    INSERT INTO
      vline.vic_gda9455_rail_vline_speeds (hour, train, speed, geom)
    SELECT
      ",as.integer(str_sub(animate.seq.hr[i],1,2))," AS hour, vic_gda9455_trains_speeds_",str_sub(animate.seq.hr[i],1,2),".train AS train, vic_gda9455_trains_speeds_",str_sub(animate.seq.hr[i],1,2),".speed AS speed, vic_gda9455_trains_speeds_",str_sub(animate.seq.hr[i],1,2),".geom AS geom
    FROM
      vline.vic_gda9455_trains_speeds_",str_sub(animate.seq.hr[i],1,2),"
    WHERE
      speed <> 'Infinity';
  "))
}


for(i in non.null.trains.hr){
  dbGetQuery(con,paste0("
    CREATE TABLE vline.vic_gda9455_total_trains_",str_sub(animate.seq.hr[i],1,2)," AS
    SELECT g.id AS id, COUNT(DISTINCT p.train) AS count
    FROM gis_victoria.vic_gda9455_admin_state_1kmgrid AS g, vline.vic_gda9455_trains_plines_",str_sub(animate.seq.hr[i],1,2)," AS p
    WHERE ST_Intersects(p.geom, g.geom)
    GROUP BY g.id
    ORDER BY g.id;
  "))
}

# dbGetQuery(con,paste0("
#   CREATE TABLE vline.vic_gda9455_hourly_trains AS
#   SELECT
#     vic_gda9455_admin_state_1kmgrid.id AS id,
#     vic_gda9455_total_trains_00.count AS hr00_01,
#     vic_gda9455_total_trains_01.count AS hr01_02,
#     vic_gda9455_total_trains_04.count AS hr04_05,
#     vic_gda9455_total_trains_05.count AS hr05_06,
#     vic_gda9455_total_trains_06.count AS hr06_07,
#     vic_gda9455_total_trains_07.count AS hr07_08,
#     vic_gda9455_total_trains_08.count AS hr08_09,
#     vic_gda9455_total_trains_09.count AS hr09_10,
#     vic_gda9455_total_trains_10.count AS hr10_11,
#     vic_gda9455_total_trains_11.count AS hr11_12,
#     vic_gda9455_total_trains_12.count AS hr12_13,
#     vic_gda9455_total_trains_13.count AS hr13_14,
#     vic_gda9455_total_trains_14.count AS hr14_15,
#     vic_gda9455_total_trains_15.count AS hr15_16,
#     vic_gda9455_total_trains_16.count AS hr16_17,
#     vic_gda9455_total_trains_17.count AS hr17_18,
#     vic_gda9455_total_trains_18.count AS hr18_19,
#     vic_gda9455_total_trains_19.count AS hr19_20,
#     vic_gda9455_total_trains_20.count AS hr20_21,
#     vic_gda9455_total_trains_21.count AS hr21_22,
#     vic_gda9455_total_trains_22.count AS hr22_23,
#     vic_gda9455_total_trains_23.count AS hr23_24,
#     vic_gda9455_admin_state_1kmgrid.egk AS egk,
#     ST_X(ST_Centroid(vic_gda9455_admin_state_1kmgrid.geom)) AS X,
#     ST_Y(ST_Centroid(vic_gda9455_admin_state_1kmgrid.geom)) AS Y
#   FROM
#     gis_victoria.vic_gda9455_admin_state_1kmgrid
#   LEFT JOIN vline.vic_gda9455_total_trains_00 ON (vic_gda9455_admin_state_1kmgrid.id = vic_gda9455_total_trains_00.id)
#   LEFT JOIN vline.vic_gda9455_total_trains_01 ON (vic_gda9455_admin_state_1kmgrid.id = vic_gda9455_total_trains_01.id)
#   LEFT JOIN vline.vic_gda9455_total_trains_04 ON (vic_gda9455_admin_state_1kmgrid.id = vic_gda9455_total_trains_04.id)
#   LEFT JOIN vline.vic_gda9455_total_trains_05 ON (vic_gda9455_admin_state_1kmgrid.id = vic_gda9455_total_trains_05.id)
#   LEFT JOIN vline.vic_gda9455_total_trains_06 ON (vic_gda9455_admin_state_1kmgrid.id = vic_gda9455_total_trains_06.id)
#   LEFT JOIN vline.vic_gda9455_total_trains_07 ON (vic_gda9455_admin_state_1kmgrid.id = vic_gda9455_total_trains_07.id)
#   LEFT JOIN vline.vic_gda9455_total_trains_08 ON (vic_gda9455_admin_state_1kmgrid.id = vic_gda9455_total_trains_08.id)
#   LEFT JOIN vline.vic_gda9455_total_trains_09 ON (vic_gda9455_admin_state_1kmgrid.id = vic_gda9455_total_trains_09.id)
#   LEFT JOIN vline.vic_gda9455_total_trains_10 ON (vic_gda9455_admin_state_1kmgrid.id = vic_gda9455_total_trains_10.id)
#   LEFT JOIN vline.vic_gda9455_total_trains_11 ON (vic_gda9455_admin_state_1kmgrid.id = vic_gda9455_total_trains_11.id)
#   LEFT JOIN vline.vic_gda9455_total_trains_12 ON (vic_gda9455_admin_state_1kmgrid.id = vic_gda9455_total_trains_12.id)
#   LEFT JOIN vline.vic_gda9455_total_trains_13 ON (vic_gda9455_admin_state_1kmgrid.id = vic_gda9455_total_trains_13.id)
#   LEFT JOIN vline.vic_gda9455_total_trains_14 ON (vic_gda9455_admin_state_1kmgrid.id = vic_gda9455_total_trains_14.id)
#   LEFT JOIN vline.vic_gda9455_total_trains_15 ON (vic_gda9455_admin_state_1kmgrid.id = vic_gda9455_total_trains_15.id)
#   LEFT JOIN vline.vic_gda9455_total_trains_16 ON (vic_gda9455_admin_state_1kmgrid.id = vic_gda9455_total_trains_16.id)
#   LEFT JOIN vline.vic_gda9455_total_trains_17 ON (vic_gda9455_admin_state_1kmgrid.id = vic_gda9455_total_trains_17.id)
#   LEFT JOIN vline.vic_gda9455_total_trains_18 ON (vic_gda9455_admin_state_1kmgrid.id = vic_gda9455_total_trains_18.id)
#   LEFT JOIN vline.vic_gda9455_total_trains_19 ON (vic_gda9455_admin_state_1kmgrid.id = vic_gda9455_total_trains_19.id)
#   LEFT JOIN vline.vic_gda9455_total_trains_20 ON (vic_gda9455_admin_state_1kmgrid.id = vic_gda9455_total_trains_20.id)
#   LEFT JOIN vline.vic_gda9455_total_trains_21 ON (vic_gda9455_admin_state_1kmgrid.id = vic_gda9455_total_trains_21.id)
#   LEFT JOIN vline.vic_gda9455_total_trains_22 ON (vic_gda9455_admin_state_1kmgrid.id = vic_gda9455_total_trains_22.id)
#   LEFT JOIN vline.vic_gda9455_total_trains_23 ON (vic_gda9455_admin_state_1kmgrid.id = vic_gda9455_total_trains_23.id)
#   WHERE (
#     vic_gda9455_total_trains_00.count IS NOT NULL OR
#     vic_gda9455_total_trains_01.count IS NOT NULL OR
#     vic_gda9455_total_trains_04.count IS NOT NULL OR
#     vic_gda9455_total_trains_05.count IS NOT NULL OR
#     vic_gda9455_total_trains_06.count IS NOT NULL OR
#     vic_gda9455_total_trains_07.count IS NOT NULL OR
#     vic_gda9455_total_trains_08.count IS NOT NULL OR
#     vic_gda9455_total_trains_09.count IS NOT NULL OR
#     vic_gda9455_total_trains_10.count IS NOT NULL OR
#     vic_gda9455_total_trains_11.count IS NOT NULL OR
#     vic_gda9455_total_trains_12.count IS NOT NULL OR
#     vic_gda9455_total_trains_13.count IS NOT NULL OR
#     vic_gda9455_total_trains_14.count IS NOT NULL OR
#     vic_gda9455_total_trains_15.count IS NOT NULL OR
#     vic_gda9455_total_trains_16.count IS NOT NULL OR
#     vic_gda9455_total_trains_17.count IS NOT NULL OR
#     vic_gda9455_total_trains_18.count IS NOT NULL OR
#     vic_gda9455_total_trains_19.count IS NOT NULL OR
#     vic_gda9455_total_trains_20.count IS NOT NULL OR
#     vic_gda9455_total_trains_21.count IS NOT NULL OR
#     vic_gda9455_total_trains_22.count IS NOT NULL OR
#     vic_gda9455_total_trains_23.count IS NOT NULL
#     );
#   "))
# 
# 
# 
# days <- colnames(final_pt_times)[6:12]
# 
# #final_pt_times <- as.data.table(read.csv(file = "final_pt_times.csv", sep=","))
# 
# train_rts_list <- unique(final_pt_times[,trip_id])
# 
# registerDoMC(7)
# 
# pts_on_rts <- foreach(i = 1:length(train_rts_list), .packages = c("data.table"), .combine = rbind) %dopar% {
#   as.data.table(cbind("ROUTE"=paste(train_rts_list[i]), "N"=as.numeric(final_pt_times[trip_id==train_rts_list[i] & thursday==1, .N])))
# }
# 
# rts_per_day <- foreach(i = 1:length(days), .packages = c("data.table"), .combine = rbind) %dopar% {
#   d <- days[i]
#   as.data.table(cbind("DAY"=paste(days[i]), "N"=nrow(final_pt_times[final_pt_times[[paste(days[i])]] == 1, .N, by=trip_id])))
# }
# 
# plot(trains.hour$HOUR,trains.hour$TRAINS)

