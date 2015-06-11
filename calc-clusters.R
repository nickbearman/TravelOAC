#calc clusters
#uses pop_centriods_data_census from census-data.R

#separate out data to be clustered
#cluster.data <-pop_centriods_data_census[, c(8,10,12,14,16:46)]
#cluster.data <-pop_centriods_data_census[, c(6,8,10,12,14:38)]

#UPDATE
cluster.data <-pop_centriods_data_census[, c(4,7,9,12,15:41)]

#scale data
cluster.data <- scale(cluster.data) #scale #everything is asinh(sq), and then zscore

# Determine number of clusters
wss <- (nrow(cluster.data)-1)*sum(apply(cluster.data,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(cluster.data,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares", main = "K-means: distance from mean by cluster amount", cex.axis = 0.90, lwd = 2)
abline(v=c(1:15), lty=1.5, lwd=0.5, col=336) 

# K-Means (trying 8 clusters)
clusters <- list()
fit <- NA
for (i in 1:10){
  print(paste("starting run", i, sep=" "))
  class.7 <- kmeans(x=cluster.data, centers=8, iter.max=1000000, nstart=1)
  fit[i] <- class.7$tot.withinss
  if (fit[i] < min(fit[1:(i-1)])){
    clusters <- class.7}
}

kfit5 <- clusters
kfit5$size #show size

k5_mvalues<- aggregate(cluster.data,by=list(kfit5$cluster),FUN=mean)



library(reshape)
#k5_mvalues_N <- rename(k5_mvalues, c(Major_PC = "Major Road Ratio", Dens_BU_SIN = "Net Density", Group.1 = "Cluster",  
#                                       NSeC_Up_2 = "NSeC (Upper)", 
#                                       Green_Ratio_SIN = "Green Space Ratio", Road_Ratio_SIN = "Street Network Ratio" ))
#k5_mvalues_N <- as.data.frame(k5_mvalues_N[, c(1,5,3,4,6,2)]) #reorder
k5_mvalues_N <- as.data.frame(k5_mvalues[,])

#do subsets
k5_mvalues_N <- as.data.frame(k5_mvalues[,c(1,2:6)]) #distances
k5_mvalues_N <- as.data.frame(k5_mvalues[,c(1,7:11)]) #cars in household
k5_mvalues_N <- as.data.frame(k5_mvalues[,c(1,12:17)]) #age
k5_mvalues_N <- as.data.frame(k5_mvalues[,c(1,18:25)]) #Ns.sec
k5_mvalues_N <- as.data.frame(k5_mvalues[,c(1,26:36)]) #travel to work mode


k5df <- melt(k5_mvalues_N ,  id = "Group.1", variable_name = "Variables")

#save mean cluster varitaion
#write.csv(k5df, "20150226-mean-cluster-variation.csv")
write.csv(k5df, "20150227-mean-cluster-variation.csv")

require(ggplot2)
ggmeans <- ggplot(k5df, aes(Group.1,value)) + geom_line(aes(colour = Variables)) + 
  scale_x_continuous(breaks=seq(1, 7, by=1)) + theme(text = element_text(size=20))
ggmeans + ggtitle("Mean variation within clusters")

#Append cluster-OAC

OA_data_centriods <- pop_centriods_data_census
OA_data_centriods$cluster <- kfit5$cluster
#write.csv(OA_data_centriods, file = "20141103-OA-final.csv")
#write.csv(OA_data_centriods, file = "20150213-OA-final.csv")
#write.csv(OA_data_centriods, file = "20150225-OA-final.csv")
write.csv(OA_data_centriods, file = "20150227-OA-final.csv")

OA_final <- data.frame(OA_data_centriods$OA11CD, kfit5$cluster)
#OA_final <- cbind(cluster_names,cluster_value)
#make shapefile @data for visual

#export as CSV

write.csv(OA_final, file = "20150225-OA-final.csv")

#look at cluster data
#list
as.data.frame(table(kfit5$cluster))
as.data.frame(table(OA_final$kfit5.cluster))

#read in shape file, join and save
  #Load Library
  library(maptools)
  setwd("~/Box Sync/data/ons/oa-2011/Output_areas_(E+W)_2011_Boundaries_(Generalised_Clipped)_V2")
  #read pop_centriods
  OA_shape <- readShapeSpatial("OA_2011_EW_BGC_V2", proj4string = CRS("+init=epsg:27700"))
  #reset wd
  setwd("C:/Users/nbearman/Dropbox/travel-oac/r-code")
  setwd("~/Dropbox/travel-oac/r-code")
  #merge
  OA_shape_cluster <- OA_shape
  OA_shape_cluster@data <- merge(OA_shape_cluster@data, OA_final, by.x = "OA11CD", by.y = "pop_centriods_data_census.OA11CD", all.x = TRUE)
  
  #export as CSV
  #write.csv(OA_final, file = "20141103-OA-final.csv")
  write.csv(OA_final, file = "20150213-OA-final.csv")

#from Alekos (copy)



k5_class <- as.data.frame.matrix(table(OA_final5[, c(3,2)]))
colnames(k5_class) <- row.names(temp) #temp = CL1 names
k5_class$Cluster <- 1:5
k5_cluster <- melt(k5_class , id = "Cluster", variable_name = "OAC")
k5_cluster <- rename(k5_cluster, c(value = "Number"))

ggmeans2 <- ggplot(k5_cluster, aes(Cluster,Number)) + geom_line(aes(colour = OAC)) + scale_x_continuous(breaks=seq(1, 5, by=1)) + theme(text = element_text(size=20))
ggmeans2 + ggtitle("Total amount (OAC) within clusters")

#as a percentage

k5_class_agg <- k5_class
k5_class_agg$Sum <- rowSums(k5_class)
k5_class_agg[, 1:7] <- k5_class_agg[, 1:7]/k5_class_agg$Sum
k5_class_agg$PC_Sum <- rowSums(k5_class_agg[, 1:7])
k5_class_pc <- k5_class_agg[,1:8]

k5_cluster_pc <- melt(k5_class_pc , id = "Cluster", variable_name = "OAC")
k5_cluster_pc <- rename(k5_cluster_pc, c(value = "Percentage"))
ggmeans3 <- ggplot(k5_cluster_pc, aes(Cluster, Percentage)) + geom_line(aes(colour = OAC)) + scale_x_continuous(breaks=seq(1, 5, by=1)) + theme(text = element_text(size=20))
ggmeans3 + ggtitle("Percentage (OAC) within clusters")

#make shapefile @data for visual
OAC_k5 <- OAC2
OAC_k5@data <- merge(OAC_k5@data, OA_final5[, c(1,3)], by.x ="OA_CODE", by.y = "All_Attr.OA_CODE", all.x = T)
library(maptools)
writePolyShape(x=OAC_k5, fn="OAC_k5_v5")

# Scatterplot Matrices from the glus Package
library(gclus)

dta_r <- abs(cor(OAC_Input52b)) # get correlations
dta_col <- dmat.color(dta_r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
dta_o <- order.single(dta_r)
cpairs(OAC_Input52b, dta_o, panel.colors=dta_col, gap=.5,
       main="Variables Ordered and Colored by Correlation" ) 

cpairs(OAC_Input52b, dta_o, panel.colors=dta_col, gap=.5,
       main="Variables Ordered and Colored by Correlation" ) 
