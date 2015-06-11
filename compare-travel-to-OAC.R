#Code to compare travel variables to OAC classification of OAs

#load OAC
OAC <-  read.csv("C:/Users/nbearman/Box Sync/data/ons/oac-2011/2011_EW_OAC_Clusters.csv")

#setup Travel OAC
TravelOAC <- OA_data_centriods #(from calc-clusters.R)
TravelOAC <- TravelOAC[,c(1,18)]

#Travel Variables
Distance
CarVan Owner
TravelToWork method
pop_centriods_data_census[1,]

data <- pop_centriods_data_census[,c(1,17:21)]
library(reshape)
data_melt <- melt(data ,  id = "OA11CD", variable_name = "Variables")

require(ggplot2)
ggmeans <- ggplot(data_melt, aes(OA11CD,value)) + geom_line(aes(colour = Variables)) + 
  scale_x_continuous(breaks=seq(1, 7, by=1)) + theme(text = element_text(size=20))
ggmeans + ggtitle("Mean variation within clusters")

require(ggplot2)
ggmeans <- ggplot(k5df, aes(Group.1,value)) + geom_line(aes(colour = Variables)) + 
  scale_x_continuous(breaks=seq(1, 7, by=1)) + theme(text = element_text(size=20))
ggmeans + ggtitle("Mean variation within clusters")