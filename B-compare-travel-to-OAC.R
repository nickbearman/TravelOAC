#File B-compare-travelOAC-to-OAC.R
#Code to compare TravelOAC data to OAC
#Part of TravelOAC GitHub Repository, see Readme file for detail

#Library
  library(reshape)
  library(ggplot2)

#load OAC data
  OAC <-  read.csv("C:/Users/nbearman/Box Sync/data/ons/oac-2011/2011_EW_OAC_Clusters.csv")

#setup Travel OAC
  TravelOAC <- OA_data_centriods #(from 3-calc-clusters.R)
  TravelOAC <- TravelOAC[,c(1,18)]

#extract data for comparision
  data <- pop_centriods_data_census[,c(1,17:21)]
  data_melt <- melt(data ,  id = "OA11CD", variable_name = "Variables")
  
#plot graph OAC
  ggmeans <- ggplot(data_melt, aes(OA11CD,value)) + geom_line(aes(colour = Variables)) + 
    scale_x_continuous(breaks=seq(1, 7, by=1)) + theme(text = element_text(size=20))
  ggmeans + ggtitle("Mean variation within clusters")

#plot graph
  ggmeans <- ggplot(k5df, aes(Group.1,value)) + geom_line(aes(colour = Variables)) + 
    scale_x_continuous(breaks=seq(1, 7, by=1)) + theme(text = element_text(size=20))
  ggmeans + ggtitle("Mean variation within clusters")