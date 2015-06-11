#Routing

#Get lat/long for OAs and stops

setwd("C:/Users/nbearman/Dropbox/travel-oac/r-code")

pop_centriods_data <- pop_centriods_data_all
rm(pop_centriods_data_census)

pop_centriods_data$OA11CD <- NULL
pop_centriods_data_census <- merge(pop_centriods_data, census, by.x = "OA11CD", by.y = "GeographyCode", all.x = TRUE)
pop_centriods_data_census[1,]

#read in data (currently sample)
rm(pop_centriods_data_all)
load("pop-data-centriods-20141020-1024.RData")
  pop_centriods_data_all <- pop_centriods_data
load("pop-data-centriods-20141020-1407-10k1-40k.RData")
  pop_centriods_data_all[10001:40000,] <- pop_centriods_data
load("pop-data-centriods-20141021-1539-40k1-70k.RData")
  pop_centriods_data_all[40001:70000,] <- pop_centriods_data
load("pop-data-centriods-20141021-1539-70k1-100k.RData")
  pop_centriods_data_all[70001:100000,] <- pop_centriods_data
load("pop-data-centriods-20141022-0820-100k1-130k.RData") ###### error
  pop_centriods_data_all[100001:130000,] <- pop_centriods_data
load("pop-data-centriods-20141022-1630-130k1-160k.RData") ###### error
  pop_centriods_data_all[130001:160000,] <- pop_centriods_data
load("pop-data-centriods-20141023-1505-160k1-170k.RData")
  pop_centriods_data_all[160001:170000,] <- pop_centriods_data
load("pop-data-centriods-20141023-1507-170k1-180k.RData")
  pop_centriods_data_all[170001:180000,] <- pop_centriods_data
load("pop-data-centriods-20141023-1714-180k1-181408.RData")
  pop_centriods_data_all[180001:181408,] <- pop_centriods_data
#save data
save(pop_centriods_data_all, file = "pop-centriods-data-all-20141024.RData")
save(pop_centriods_data_all, file = "pop-centriods-data-all-20141121.RData")
#load pop all
load("pop-centriods-data-all-20141024.RData")
#use pop_centriods_data_all as pop_centriods_data for next step
pop_centriods_data <- pop_centriods_data_all

#get lat long for stops
stops.ll <- stops[,c(2,6,31:32)]
#airport
  #match to data
  pop_centriods_data <- merge(pop_centriods_data, stops.ll, by.x = "airport.atco.code", by.y = "AtcoCode")
  #del columns
  pop_centriods_data$OA11CD <- NULL
  pop_centriods_data$coords.x1 <- NULL
  pop_centriods_data$coords.x2 <- NULL
  #rename columns
  colnames(pop_centriods_data)[colnames(pop_centriods_data)=="CommonName"] <- "AirportCommonName"
  colnames(pop_centriods_data)[colnames(pop_centriods_data)=="Longitude"] <- "AirportLongitude"
  colnames(pop_centriods_data)[colnames(pop_centriods_data)=="Latitude"] <- "AirportLatitude"
#rail
  #match to data
  pop_centriods_data <- merge(pop_centriods_data, stops.ll, by.x = "rail.atco.code", by.y = "AtcoCode")
  #rename columns
  colnames(pop_centriods_data)[colnames(pop_centriods_data)=="CommonName"] <- "RailCommonName"
  colnames(pop_centriods_data)[colnames(pop_centriods_data)=="Longitude"] <- "RailLongitude"
  colnames(pop_centriods_data)[colnames(pop_centriods_data)=="Latitude"] <- "RailLatitude"
#ferry
  #match to data
  pop_centriods_data <- merge(pop_centriods_data, stops.ll, by.x = "ferry.atco.code", by.y = "AtcoCode")
  #rename columns
  colnames(pop_centriods_data)[colnames(pop_centriods_data)=="CommonName"] <- "FerryCommonName"
  colnames(pop_centriods_data)[colnames(pop_centriods_data)=="Longitude"] <- "FerryLongitude"
  colnames(pop_centriods_data)[colnames(pop_centriods_data)=="Latitude"] <- "FerryLatitude"  
#tram
  #match to data
  pop_centriods_data <- merge(pop_centriods_data, stops.ll, by.x = "tram.atco.code", by.y = "AtcoCode")
  #rename columns
  colnames(pop_centriods_data)[colnames(pop_centriods_data)=="CommonName"] <- "TramCommonName"
  colnames(pop_centriods_data)[colnames(pop_centriods_data)=="Longitude"] <- "TramLongitude"
  colnames(pop_centriods_data)[colnames(pop_centriods_data)=="Latitude"] <- "TramLatitude"  
#save all non bus
  save(pop_centriods_data, file = "pop_centriods_data_ll_nonbus_20141024.RData")

#use pop_centriods_data_all as pop_centriods_data for next step
pop_centriods_data <- pop_centriods_data_all
pop_centriods_data <- pop_centriods_data_all[1:100000,]
pop_centriods_data <- pop_centriods_data_all[100001:180000,]


#cut down data
pop_centriods_data <- pop_centriods_data[,c(1,16:17)]
#bus
  #match to data
  pop_centriods_data <- merge(pop_centriods_data, stops.ll, by.x = "bus.atco.code", by.y = "AtcoCode")
  #rename columns
  colnames(pop_centriods_data)[colnames(pop_centriods_data)=="CommonName"] <- "BusCommonName"
  colnames(pop_centriods_data)[colnames(pop_centriods_data)=="Longitude"] <- "BusLongitude"
  colnames(pop_centriods_data)[colnames(pop_centriods_data)=="Latitude"] <- "BusLatitude"  

pop_centriods_dataA <- pop_centriods_data

#Get lat-long for OAs
  #reproject pop centriods
  library(rgdal)
  pop_centriods_ll <- spTransform(pop_centriods, CRS("+init=epsg:4326"))
  #create non spatial frame
  pop_centriods_ll_data <- cbind(pop_centriods_ll@data,pop_centriods_ll@coords)
  #remove extra columns
  pop_centriods_data$OA11CD <- NULL
  #merge
  pop_centriods_data <- merge(pop_centriods_data, pop_centriods_ll_data, by.x = "OA11CD", by.y = "OA11CD", all.x = TRUE)
  #rename cols  
  colnames(pop_centriods_data)[colnames(pop_centriods_data)=="coords.x1.y"] <- "OALongitude"
  colnames(pop_centriods_data)[colnames(pop_centriods_data)=="coords.x2.y"] <- "OALatitude"

#save example
  #save(pop_centriods_data, file = "20141121-sample-40k1-70k.RData")
  #save(pop_centriods_data, file = "20141121-sample-40k1-70k-with_census.RData")


#temp routing example

# notes using 40k1-70k example comibine with census is fine. will need to check all otehr data and rerun where necessary.

router <- paste0("router --dir=test/routino-exp/data --transport=",transport," --prefix=gb --quickest  --lon1=",pupil_lon," --lat1=",pupil_lat," --lon2=",school_lon," --lat2=",school_lat," --quiet --output-text-all --profiles=/Users/nickbearman/Documents/home-school-project/r-coding/routino_profile.xml")

router <- paste0("/Users/nickbearman/Documents/routino/routino-2.7.2/web/bin/router --dir=../data --transport=foot --quickest  --lon1=-2.345 --lat1=54.321 --lon2=-2.233 --lat2=54.221 --quiet --output-text-all --profiles=/Users/nickbearman/Documents/routino/routino-2.7.2/web/data/routino_profile.xml")

setwd("~/Documents/routino/routino-2.7.2/web/bin/")

#run code
system(router)




