#File 2-census-data.R
#Code to read in and process census data for use in k-means clustering and join census data to nearest-stop-data
#Written by Nick Bearman, 20150227
#Part of TravelOAC GitHub Repository, see Readme file for details
#Previous file 1-calc-nearest-stop.R

#load existing distance data
  #setwd("~/Dropbox/travel-oac/r-code")
  setwd("/Users/nickbearman/Box Sync/travel-oac/r-code")
  #load("pop-centriods-data-all-20141024.RData")
  load("pop-data-centriods-20150224-1257.RData")
  #get sample (useful for when we don't want to process all the data)
    pop_centriods_data <- pop_centriods_data_all[1:10,]
  #or use all
    pop_centriods_data <- pop_centriods_data_all
  #remove extra column OA11CD
    #pop_centriods_data$OA11CD <- NULL
    #need to remove column 3 (OA11CD) not column 1 (OA11CD)
    pop_centriods_data <- pop_centriods_data[,c(1:3,7:18)]

#read in census data
  #OSX
    car.van <- read.csv("/Users/nickbearman/Box Sync/data/nomis/2011-census-bulk/KS404ew_2011_oa-car-van/KS404EWDATA.csv")
    age <-  read.csv("/Users/nickbearman/Box Sync/data/nomis/2011-census-bulk/ks102ew_2011_oa-age/KS102ew_2011_oa/KS102EWDATA.csv")
    ns.sec <- read.csv("/Users/nickbearman/Box Sync/data/nomis/2011-census-bulk/KS611ew_2011_oa/KS611EWDATA.CSV")
    travel.work <- read.csv("/Users/nickbearman/Box Sync/data/nomis/2011-census-bulk/qs701ew_2011_oa/QS701EWDATA.csv")
    gender <- read.csv("/Users/nickbearman/Box Sync/data/nomis/2011-census-bulk/ks101ew_2011oa/KS101EWDATA.csv")
  #win
    car.van <- read.csv("C:/Users/nbearman/Box Sync/data/nomis/2011-census-bulk/KS404ew_2011_oa-car-van/KS404EWDATA.csv")
    age <-  read.csv("C:/Users/nbearman/Box Sync/data/nomis/2011-census-bulk/KS102ew_2011_oa-age/KS102ew_2011_oa/KS102EWDATA.csv")
    ns.sec <- read.csv("C:/Users/nbearman/Box Sync/data/nomis/2011-census-bulk/KS611ew_2011_oa/KS611EWDATA.CSV")
    travel.work <- read.csv("C:/Users/nbearman/Box Sync/data/nomis/2011-census-bulk/qs701ew_2011_oa/QS701EWDATA.csv")
    gender <- read.csv("C:/Users/nbearman/Box Sync/data/nomis/2011-census-bulk/ks101ew_2011oa/KS101EWDATA.csv")

#tidy variables for each mode of transport
  #car
    car.van <- car.van[,c(1,9:13)] #remove extras
    colnames(car.van) <- c("GeographyCode","CarVan0","CarVan1","CarVan2","CarVan3","CarVan4plus") #rename
    #combine 2, 3 and 4+ into 2+
      car.van$CarVan2plus <- (car.van$CarVan2 + car.van$CarVan3 + car.van$CarVan4plus)
      car.van$CarVan2 <- NULL
      car.van$CarVan3 <- NULL
      car.van$CarVan4plus <- NULL
    car.van[,c(2:4)] <- car.van[,c(2:4)]/100 #convert to pc
  #Age
    #Aggregate groups
      age$total <- age$KS102EW0001
      age$age_0.4 <- age$KS102EW0002
      age$age_5.14 <- age$KS102EW0003 + age$KS102EW0004 + age$KS102EW0005
      #age$age_15.24 <- age$KS102EW0006 + age$KS102EW0007 + age$KS102EW0008 + age$KS102EW0009
      #age$age_25.44 <- age$KS102EW0010 + age$KS102EW0011
      age$age_15.44 <- age$KS102EW0006 + age$KS102EW0007 + age$KS102EW0008 + age$KS102EW0009 + age$KS102EW0010 + age$KS102EW0011
      age$age_45.64 <- age$KS102EW0012 + age$KS102EW0013
      age$age_65p <- age$KS102EW0014 + age$KS102EW0015 + age$KS102EW0016 + age$KS102EW0017
    #calc pc
      age$pc_age_0.4 <- age$age_0.4 / age$total
      age$pc_age_5.14 <- age$age_5.14 / age$total
      #age$pc_age_15.24 <- age$age_15.24 / age$total
      #age$pc_age_25.44 <- age$age_25.44 / age$total
      age$pc_age_15.44 <- age$age_15.44 / age$total
      age$pc_age_45.64 <- age$age_45.64 / age$total
      age$pc_age_65p <- age$age_65p / age$total
    #remove extra rows
      age <- age[c(43:47)]
  #NS.Sec
    ns.sec <- ns.sec[,c(18,21:27)] #remove extras
    ns.sec <- ns.sec/100 #convert to pc
    colnames(ns.sec) <- c("ns1.highermgt","ns2.lwrmgr","ns3.inter","ns4.smemp","ns5.lowersup","ns6.semi.rout","ns7.routine","ns8.never") #rename
    #combine 1&2 and 6&7
      ns.sec$ns1.2mgr <- ns.sec$ns1.highermgt + ns.sec$ns2.lwrmgr
      ns.sec$ns6.7rout <- ns.sec$ns6.semi.rout + ns.sec$ns7.routine
      ns.sec$ns1.highermgt <- NULL
      ns.sec$ns2.lwrmgr <- NULL
      ns.sec$ns6.semi.rout <- NULL
      ns.sec$ns7.routine <- NULL
  #Means of Travel to work
    travel.work$travelpc.home <- travel.work$QS701EW0002 / travel.work$QS701EW0001
    travel.work$travelpc.tram <- travel.work$QS701EW0003 / travel.work$QS701EW0001
    travel.work$travelpc.train <- travel.work$QS701EW0004 / travel.work$QS701EW0001
    travel.work$travelpc.bus <- travel.work$QS701EW0005 / travel.work$QS701EW0001
    travel.work$travelpc.taxi <- travel.work$QS701EW0006 / travel.work$QS701EW0001
    travel.work$travelpc.motorcycle <- travel.work$QS701EW0007 / travel.work$QS701EW0001
    travel.work$travelpc.dricarvan <- travel.work$QS701EW0008 / travel.work$QS701EW0001
    travel.work$travelpc.pasncarvan <- travel.work$QS701EW0009 / travel.work$QS701EW0001
    travel.work$travelpc.cycle <- travel.work$QS701EW0010 / travel.work$QS701EW0001
    travel.work$travelpc.walk <- travel.work$QS701EW0011 / travel.work$QS701EW0001
    travel.work$travelpc.other <- travel.work$QS701EW0012 / travel.work$QS701EW0001
    travel.work$travelpc.notemploy <- travel.work$QS701EW0013 / travel.work$QS701EW0001
    #remove extra rows
    travel.work <- travel.work[c(15:25)]
  #gender
    gender <- gender[,c(1:2,4)] #remove extras
    colnames(gender) <- c("GeographyCode","Total","Females") #rename
    gender$pc.female <-  (gender$Females/gender$Total) #convert to pc
    gender$Total <- NULL
    gender$Females <- NULL
    gender$GeographyCode <- NULL
  #cbind census variables
    census <- cbind(car.van, age,ns.sec,travel.work,gender)
  #remove intermediate variables
    rm(age)
    rm(car.van)
    rm(ns.sec)
    rm(travel.work)
    rm(gender)

#merge data with dist to cloest stop
  pop_centriods_data_census <- merge(pop_centriods_data, census, by.x = "OA11CD", by.y = "GeographyCode", all.x = TRUE)

#save matrix of census data and OAs
  save(pop_centriods_data_census, file = "20150227_census_data_OAs.Rdata")
#calc correlation
  pop_centriods_data_census_cor <- pop_centriods_data_census
  pop_centriods_data_census_cor$OA11CD <- NULL
  pop_centriods_data_census_cor <- cor(pop_centriods_data_census_cor, use = "all.obs")
  write.csv(pop_centriods_data_census_cor, file = "20150227-pop_centriods_data_census_cor.csv")

#check out census data correlation (also need to look at distances too)
  cor(census$CarVan0, census$CarVan1)
  d <- data.frame(census$CarVan0,
                  census$CarVan1,
                  census$CarVan2)
  e <- data.frame(census[2:25,])
  cor(d, use = "all.obs") # get correlations
  as.matrix(cor(d)) # put correlations in a matrix

#next file is 3-calc-clusters.r
