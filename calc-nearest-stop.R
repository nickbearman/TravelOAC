

#Import Output_areas_(E+W)_2011_Population_Weighted_Centroids_V2
  #Load Library
  library(maptools)
  #Set WD
  setwd("C:/Users/nbearman/Box Sync/data/ons/oa-2011/2011 OA population weighted centroids/Output_areas_(E+W)_2011_Population_Weighted_Centroids_V2")
  setwd("~/Box Sync/data/ons/oa-2011/2011 OA population weighted centroids/Output_areas_(E+W)_2011_Population_Weighted_Centroids_V2")
  #read pop_centriods
  pop_centriods <- readShapeSpatial("OA_2011_EW_PWC", proj4string = CRS("+init=epsg:27700"))
  #reset wd
  setwd("C:/Users/nbearman/Dropbox/travel-oac/r-code")

#temp test
  setwd("C:/Users/nbearman/Dropbox/travel-oac/r-code")
  
  #read pop_centriods
  pop_centriods <- readShapeSpatial("test-oa", proj4string = CRS("+init=epsg:27700"))
  #reset wd
  setwd("C:/Users/nbearman/Dropbox/travel-oac/r-code")
    
#Import station locations from NaPTAN
  #Set WD
  setwd("C:/Users/nbearman/Box Sync/data/data.gov.uk/naptan/NaPTANcsv/derived-data")
  setwd("~/Box Sync/data/data.gov.uk/naptan/NaPTANcsv/derived-data")
  #read stops data
  stops <- read.csv("Stops-with-code.csv", header = TRUE) # data is fine for stop locations, but not number of stops as there are duplicates
  #reset wd
  setwd("C:/Users/nbearman/Dropbox/travel-oac/r-code")  
  #Count by mode type
  #not used
  #Rail data has the prefix 910,Air 920, Ferry 930, Metro/Tram 940. I assume everything else is bus. 
  #as.data.frame(table(stops$ModeCode))
  #check stop type
  #as.data.frame(table(stops$StopType))
#Split data into rail, air, ferry, metro/tram, bus
#  rail.stops <- stops[stops$ModeCode == "910",]
#  air.stops <- stops[stops$ModeCode == "920",] 
#  air.stops <- stops[stops$StopType == "AIR",] 
#  ferry.stops <- stops[stops$ModeCode == "930",] 
 # tram.stops <- stops[stops$ModeCode == "940",]
#  bus.stops <- stops[stops$ModeCode < "910",] 
  
#Split data into rail, air, ferry, metro/tram, bus
  rail.stops <- stops[stops$StopType == "RSE" | stops$StopType == "RLY" | stops$StopType == "RPL",]
  air.stops <- stops[stops$StopType == "AIR" | stops$StopType == "GAT",] 
  ferry.stops <- stops[stops$StopType == "FER" | stops$StopType == "FBT",] 
  tram.stops <- stops[stops$StopType == "TMU" | stops$StopType == "MET" | stops$StopType == "PLT",]
  bus.stops <- stops[stops$StopType == "BCE" | stops$StopType == "BST" | stops$StopType == "BCS" | stops$StopType == "BCQ",] 
  
#for each OA, calculate nearest stop
  #new & used
  #example http://stackoverflow.com/questions/22121742/calculate-the-distance-between-two-points-of-two-datasets-nearest-neighbor
  #and http://stackoverflow.com/questions/13458702/determining-minimum-values-in-a-vector-in-r
  #create non-spatial data frame
    pop_centriods_data <- cbind(pop_centriods@data,pop_centriods@coords)

  #add lat long to OA data
    #reproject pop centriods
      library(rgdal)
      pop_centriods_ll <- spTransform(pop_centriods, CRS("+init=epsg:4326"))
    #create non spatial frame
      pop_centriods_ll_data <- cbind(pop_centriods_ll@data,pop_centriods_ll@coords)
    #remove extra columns
      pop_centriods_ll_data$OA11CD <- NULL
    #rename cols  
      colnames(pop_centriods_ll_data)[colnames(pop_centriods_ll_data)=="coords.x1"] <- "Longitude"
      colnames(pop_centriods_ll_data)[colnames(pop_centriods_ll_data)=="coords.x2"] <- "Latitude" 
    #cbind
      pop_centriods_data <- cbind(pop_centriods_data,pop_centriods_ll_data)
    #tidy  
      rm(pop_centriods_ll, pop_centriods_ll_data)
  
  #setup sample / reset sample
  #pop_centriods_data <- pop_centriods_data2[1:10000,]
  #pop_centriods_data <- pop_centriods_data2[180001:181408,]
  pop_centriods_data <- pop_centriods_data[1:30000,]
  pop_centriods_data <- pop_centriods_data[30001:40000,]
  pop_centriods_data <- pop_centriods_data[30001:31000,]
  pop_centriods_data <- pop_centriods_data[30010:30030,]
  
  #start timer
    ptm <- proc.time()  

  #extract airport loc & id
    air <- air.stops[,c(1,2,6,29:30)]
    rail <- rail.stops[,c(1,2,6,29:30)]
    ferry <- ferry.stops[,c(1,2,6,29:30)]
    tram <- tram.stops[,c(1,2,6,29:30)]
    bus <- bus.stops[,c(1,2,6,29:30)]
  #setup
  i <- 1
  airport.code <- NULL
  airport.distance <- NULL
  rail.code <- NULL
  rail.distance <- NULL
  rail.distance.route <- NULL
  ferry.code <- NULL
  ferry.distance <- NULL
  tram.code <- NULL
  tram.distance <- NULL
  tram.distance.route <- NULL
  bus.code <- NULL
  bus.distance <- NULL
  bus.distance.route <- NULL
  
  #loop
  for (i in 1:nrow(pop_centriods_data)) {
    #pick out one row
    single_oa <- pop_centriods_data[i,]
    #airport
      #calc dist
      tmp1 <- (sqrt((air$Easting-single_oa$coords.x1)^2+(air$Northing-single_oa$coords.x2)^2))
      #sort data for which airport and select out closest
      item <- which(tmp1 %in% sort(tmp1)[1])
      item <- item[1]
      airport.code[i] <- as.character(air$AtcoCode[item]) #add name to temp
      airport.distance[i] <- (sort(tmp1)[1]) #add stright line distance
    #rail
      #calc dist
      tmp1 <- (sqrt((rail$Easting-single_oa$coords.x1)^2+(rail$Northing-single_oa$coords.x2)^2))
      #sort data for which airport and select out closest
      item <- which(tmp1 %in% sort(tmp1)[1])
      item <- item[1]
      rail.code[i] <- as.character(rail$AtcoCode[item]) #add name to temp
      rail.distance[i] <- (sort(tmp1)[1]) #add stright line distance
      #rail walking route
        #get current rail stop details
        current.rail <- rail.stops[which(rail.stops$AtcoCode == rail.code[i]),]
        #set transport  
        transport <- "foot" #or set to transport <- "motorcar"
        #create command, set wd and run   
        router <- paste0("/Users/nickbearman/Documents/routino/routino-2.7.2/web/bin/router --dir=../data --transport=",transport," --quickest  --lon1=",current.rail$Longitude," --lat1=",current.rail$Latitude," --lon2=",single_oa$Longitude," --lat2=",single_oa$Latitude," --quiet --output-text-all --profiles=/Users/nickbearman/Documents/routino/routino-2.7.2/web/data/routino_profile.xml")
        setwd("~/Documents/routino/routino-2.7.2/web/bin/")
        system(router)
        #read in file to get distance
        fileloc <- "quickest-all.txt" #set file name
        #check if file exists
        suppressWarnings(try_read <- try(read.delim(fileloc, header = FALSE, sep = "\t", skip = 4)))
        # if file exists
        if ((file.exists(fileloc)) && (!inherits(try_read, 'try-error'))){
          #read in file
          routeresults <- read.delim(fileloc, header = FALSE, sep = "\t", skip = 4)
          #extract total distance
          rail.distance.route[i] <- max(routeresults$V7)*1000
          #delete file
          system(" rm quickest-all.txt")
        } else {
          #route is non routable, so return error
          rail.distance.route[i] <- -3
        }
    rm(router,fileloc,try_read,transport) #tidy
    #ferry
      #calc dist
      tmp1 <- (sqrt((ferry$Easting-single_oa$coords.x1)^2+(ferry$Northing-single_oa$coords.x2)^2))
      #sort data for which airport and select out closest
      item <- which(tmp1 %in% sort(tmp1)[1])
      item <- item[1]
      ferry.code[i] <- as.character(ferry$AtcoCode[item]) #add name to temp
      ferry.distance[i] <- (sort(tmp1)[1]) #add stright line distance
    #tram
      #calc dist
      tmp1 <- (sqrt((tram$Easting-single_oa$coords.x1)^2+(tram$Northing-single_oa$coords.x2)^2))
      #sort data for which airport and select out closest
      item <- which(tmp1 %in% sort(tmp1)[1])
      item <- item[1]
      tram.code[i] <- as.character(tram$AtcoCode[item]) #add name to temp
      tram.distance[i] <- (sort(tmp1)[1]) #add stright line distance
      #tram walking route
        #get current tram stop details
        current.tram <- tram.stops[which(tram.stops$AtcoCode == tram.code[i]),]
        #set transport  
        transport <- "foot" #or set to transport <- "motorcar"
        #create command, set wd and run   
        router <- paste0("/Users/nickbearman/Documents/routino/routino-2.7.2/web/bin/router --dir=../data --transport=",transport," --quickest  --lon1=",current.tram$Longitude," --lat1=",current.tram$Latitude," --lon2=",single_oa$Longitude," --lat2=",single_oa$Latitude," --quiet --output-text-all --profiles=/Users/nickbearman/Documents/routino/routino-2.7.2/web/data/routino_profile.xml")
        setwd("~/Documents/routino/routino-2.7.2/web/bin/")
        system(router)
        #read in file to get distance
        fileloc <- "quickest-all.txt" #set file name
        #check if file exists
        suppressWarnings(try_read <- try(read.delim(fileloc, header = FALSE, sep = "\t", skip = 4)))
        # if file exists
          if ((file.exists(fileloc)) && (!inherits(try_read, 'try-error'))){
            #read in file
            routeresults <- read.delim(fileloc, header = FALSE, sep = "\t", skip = 4)
            #extract total distance
            tram.distance.route[i] <- max(routeresults$V7)*1000
            #delete file
            system(" rm quickest-all.txt")
          } else {
            #route is non routable, so return error
            tram.distance.route[i] <- -3
          }
    rm(router,fileloc,try_read,transport) #tidy
    #bus
      #calc dist
      tmp1 <- (sqrt((bus$Easting-single_oa$coords.x1)^2+(bus$Northing-single_oa$coords.x2)^2))
      #sort data for which airport and select out closest
      item <- which(tmp1 %in% sort(tmp1)[1])
      item <- item[1]
      bus.code[i] <- as.character(bus$AtcoCode[item]) #add name to temp
      bus.distance[i] <- (sort(tmp1)[1]) #add stright line distance
      #bus walking route
        #get current bus stop details
        current.bus <- bus.stops[which(bus.stops$AtcoCode == bus.code[i]),]
      #set transport  
        transport <- "foot" #or set to transport <- "motorcar"
      #create command, set wd and run   
        router <- paste0("/Users/nickbearman/Documents/routino/routino-2.7.2/web/bin/router --dir=../data --transport=",transport," --quickest  --lon1=",current.bus$Longitude," --lat1=",current.bus$Latitude," --lon2=",single_oa$Longitude," --lat2=",single_oa$Latitude," --quiet --output-text-all --profiles=/Users/nickbearman/Documents/routino/routino-2.7.2/web/data/routino_profile.xml")
        setwd("~/Documents/routino/routino-2.7.2/web/bin/")
        system(router)
      #read in file to get distance
        fileloc <- "quickest-all.txt" #set file name
        #check if file exists
        suppressWarnings(try_read <- try(read.delim(fileloc, header = FALSE, sep = "\t", skip = 4)))
        # if file exists
        if ((file.exists(fileloc)) && (!inherits(try_read, 'try-error'))){
          #read in file
          routeresults <- read.delim(fileloc, header = FALSE, sep = "\t", skip = 4)
          #extract total distance
          bus.distance.route[i] <- max(routeresults$V7)*1000
          #delete file
          system(" rm quickest-all.txt")
        } else {
          #route is non routable, so return error
          bus.distance.route[i] <- -3
        }
    rm(router,fileloc,try_read,transport) #tidy
    #print
    print(i)
    i <- i + 1 #move to next item
  }
  rm(i,single_oa,tmp1,item) #tidy
  
  #cbind
  pop_centriods_data <- cbind(pop_centriods_data,airport.code,airport.distance,rail.code,rail.distance,rail.distance.route,ferry.code,ferry.distance,tram.code,tram.distance,tram.distance.route,bus.code,bus.distance,bus.distance.route)
  
  #stop timer
  proc.time() - ptm
  rm(ptm)
  
  
#save file
  setwd("~/Dropbox/travel-oac/r-code")
  #save(pop_centriods_data, file = "pop-data-centriods-20141128-1457.RData")
  save(pop_centriods_data, file = "pop-data-centriods-20150224-1257.RData")
  
  #next file is census-data.R
  