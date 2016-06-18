require(RPostgreSQL)
require(rgdal)
require(rgeos)
require(maptools)
require(data.table)
require(zoo)
require(doMC)
require(lubridate)
require(stringr)
require(sp)

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

#write.csv(train_rts, file = "data/train_rts.csv", row.names=FALSE)

train_rts_list <- unique(train_rts$trip_id) #create vector of unique trip ids

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

dow <- c("sunday","monday","tuesday","wednesday","thursday","friday","saturday") #create vector of days of the week for loops

##### Create segment data for count and speed of trains#####

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

SPLDF_TOT <- foreach(i = dow, .packages = c("rgdal","rgeos","maptools","data.table"), .combine=spRbind) %dopar% { #for each day of the week (train schedules vary)
  SPLDF_DAY <- vector(mode='list',length(get(paste0("train_hrs_",i)))) #initialise vector of lists for trip_id and speeds of trains
  for (j in get(paste0("train_hrs_",i))){ #load vector of hours that trains operate on specific day of week
    if (j < max(get(paste0("train_hrs_",i)))){ #check if hours are below the maximum in vector (cannot use operationals for range of unknown values)
      trips <- final_pt_times.gis[get(i)==1 & arrival_time>=animate.seq.hr[j] & arrival_time<animate.seq.hr[j+1],unique(trip_id)] #generate list of unique trips that operate on specific day of week (i) for specific hour (j)
      SPLDF <- vector(mode='list',length(trips)) #initialise vector of lists for spatial lines for each unique trip
      for(k in trips){ #loop over each unique trip
        if(nrow(final_pt_times.gis[get(i)==1 & arrival_time>=animate.seq.hr[j] & arrival_time<animate.seq.hr[j+1] & trip_id==paste(k),.(shape_id)])<=1){ #check if at least 2 points exist on trip (k) during hour (j) on day of week (i); if not, skip
          next
        }
        train.data <- final_pt_times.gis[get(i)==1 & arrival_time>=animate.seq.hr[j] & arrival_time<animate.seq.hr[j+1] & trip_id==paste(k),.(shape_id,shape_dist,x,y,arrival_time)] #extract information (plus spatial) for trip (k) during hour (j) on day of week (i)
        kph <- as.vector(rep(0.0,nrow(train.data[,.(x,y)])-1)) #initialise vector for calculated speeds on all segments of trip
        SPL <- vector(mode='list',nrow(train.data[,.(x,y)])-1) #initialise vector of lists for spatial lines on all segments of trip
        for (m in 2:nrow(train.data[,.(x,y)])){ #loop over each set of coordinates
          km <- sqrt(sum((train.data[m,.(x,y)] - train.data[m-1,.(x,y)]) ^ 2))/1000 #calculate distance between points
          hour <- abs(StrToSec(train.data$arrival_time[m])-StrToSec(train.data$arrival_time[m-1]))/3600 #calculate time in hours between points
          kph[m-1] <- km/hour #calculate speed
          if (m==2){ #operate on first segment
            SPL <- list(Lines(list(Line(train.data[c(m-1,m),.(x,y)])),paste0(k,"-",str_sub(dow[i],1,2),"-",j,"-",m-1))) #construct spatial lines and add trip identification attributes
          }else{ #for all subsequent segments
            SPL[m-1] <- list(Lines(list(Line(train.data[c(m-1,m),.(x,y)])),paste0(k,"-",str_sub(dow[i],1,2),"-",j,"-",m-1))) #construct spatial lines and add trip identification attributes
          }
        }
        lines <- SpatialLines(SPL, proj4string = CRS("+init=epsg:28355")) #combine all segments for unique trip (k) during hour (j) on day of week (i)
        data <- data.frame("train"=rep(paste(k),length(lines)),"speed"=kph,"hour"=rep(as.integer(str_sub(animate.seq.hr[j],1,2)),length(lines)),"dow"=rep(paste(i),length(lines))) #construct attribute table for spatial lines
        row.names(data) <- paste0(rep(k,length(row.names(data))),"-",rep(str_sub(dow[i],1,2),length(row.names(data))),"-",rep(j,length(row.names(data))),"-",row.names(data)) #assign row names for attribute table (required for spatial dataframe)
        SPLDF[which(trips==k)] <- SpatialLinesDataFrame(lines, data) #construct spatiallines dataframe for unique trip (k) during hour (j) on day of week (i)
      }
      SPLDF <- SPLDF[!sapply(SPLDF, is.null)] #remove any trips that do not have data or geometry
      #SPLDF2 <- do.call(rbind,SPLDF)
      SPLDF_DAY[which(get(paste0("train_hrs_",i))==j)] <- do.call(rbind,SPLDF) #combine all data for all trips for all hours for doy of week (i)
      #writeOGR(SPLDF2, ".", paste("shp_files/train_speeds_",i,"_",str_sub(animate.seq.hr[j],1,2),sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)
    
    }else{
      trips <- final_pt_times.gis[get(i)==1 & arrival_time>=animate.seq.hr[j],unique(trip_id)] #generate list of unique trips that operate on specific day of week (i) for specific hour (j)
      SPLDF <- vector(mode='list',length(trips)) #initialise vector of lists for spatial lines for each unique trip
      for(k in trips){ #loop over each unique trip
        if(nrow(final_pt_times.gis[get(i)==1 & arrival_time>=animate.seq.hr[j] & trip_id==paste(k),.(shape_id)])<=1){ #check if at least 2 points exist on trip (k) during hour (j) on day of week (i); if not, skip
          next
        }
        train.data <- final_pt_times.gis[get(i)==1 & arrival_time>=animate.seq.hr[j] & trip_id==paste(k),.(shape_id,x,y,arrival_time)] #extract information (plus spatial) for trip (k) during hour (j) on day of week (i)
        kph <- as.vector(rep(0.0,nrow(train.data[,.(x,y)])-1)) #initialise vector for calculated speeds on all segments of trip
        SPL <- vector(mode='list',nrow(train.data[,.(x,y)])-1) #initialise vector of lists for spatial lines on all segments of trip
        for (m in 2:nrow(train.data[,.(x,y)])){ #loop over each set of coordinates
          km <- sqrt(sum((train.data[m,.(x,y)] - train.data[m-1,.(x,y)]) ^ 2))/1000 #calculate distance between points
          hour <- abs(StrToSec(train.data$arrival_time[m])-StrToSec(train.data$arrival_time[m-1]))/3600 #calculate time in hours between points
          kph[m-1] <- km/hour #calculate speed
          if (m==2){ #operate on first segment
            SPL <- list(Lines(list(Line(train.data[c(m-1,m),.(x,y)])),paste0(k,"-",str_sub(dow[i],1,2),"-",j,"-",m-1))) #construct spatial lines and add trip identification attributes
          }else{
            SPL[m-1] <- list(Lines(list(Line(train.data[c(m-1,m),.(x,y)])),paste0(k,"-",str_sub(dow[i],1,2),"-",j,"-",m-1))) #construct spatial lines and add trip identification attributes
          }
        }
        lines <- SpatialLines(SPL, proj4string = CRS("+init=epsg:28355")) #combine all segments for unique trip (k) during hour (j) on day of week (i)
        data <- data.frame(train=rep(paste(k),length(lines)),speed=kph,hour=rep(as.integer(str_sub(animate.seq.hr[j],1,2)),length(lines)),dow=rep(paste(i),length(lines))) #construct attribute table for spatial lines
        row.names(data) <- paste0(rep(k,length(row.names(data))),"-",rep(str_sub(dow[i],1,2),length(row.names(data))),"-",rep(j,length(row.names(data))),"-",row.names(data)) #assign row names for attribute table (required for spatial dataframe)
        SPLDF[which(trips==k)] <- SpatialLinesDataFrame(lines, data) #construct spatiallines dataframe for unique trip (k) during hour (j) on day of week (i)
      }
      SPLDF <- SPLDF[!sapply(SPLDF, is.null)] #remove any trips that do not have data or geometry
      #SPLDF2 <- do.call(rbind,SPLDF)
      SPLDF_DAY[which(get(paste0("train_hrs_",i))==j)] <- do.call(rbind,SPLDF) #combine all data for all trips for all hours for doy of week (i)
      #writeOGR(SPLDF2, ".", paste("shp_files/train_speeds_",i,"_",str_sub(animate.seq.hr[j],1,2),sep=""), driver="ESRI Shapefile", overwrite_layer=TRUE)      
    }
  }
  do.call(rbind,SPLDF_DAY) # combine data for all days of the week
}

##### Write spatial data to database #####

writeOGR(SPLDF_TOT, dsn="PG:dbname=qaeco_spatial user=qaeco password=Qpostgres15 host=boab.qaeco.com port=5432", layer="vline.vic_gda9455_rail_vline_speeds", driver="PostgreSQL", layer_options = "geometry_name=geom", overwrite_layer=TRUE)

dbGetQuery(con,paste0("CREATE INDEX vic_gda9455_rail_vline_speeds_gix ON vline.vic_gda9455_rail_vline_speeds USING GIST (geom);"))
dbGetQuery(con,paste0("SELECT UpdateGeometrySRID('vline','vic_gda9455_rail_vline_speeds','geom',28355);"))
dbGetQuery(con,paste0("VACUUM ANALYZE vline.vic_gda9455_rail_vline_speeds;"))

