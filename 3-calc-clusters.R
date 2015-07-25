#File 3-calc-clusters
#Code to calculate the number of clusters required, and complete the K-means classification
#Written by Nick Bearman, 20150227
#Part of TravelOAC GitHub Repository, see Readme file for details
#Previous file 2-census-data.R

#load libaries
  library(reshape)
  library(ggplot2)
  library(maptools)

#Extract data for cluster classification
  #uses pop_centriods_data_census from census-data.R
  #separate out data to be clustered
  cluster.data <-pop_centriods_data_census[, c(4,7,9,12,15:41)]

#scale data
  #scale #everything is asinh(sq), and then zscore
  cluster.data <- scale(cluster.data) 

#Determine number of clusters required by elbow plot
  wss <- (nrow(cluster.data)-1)*sum(apply(cluster.data,2,var))
  for (i in 2:15) wss[i] <- sum(kmeans(cluster.data,
                                       centers=i)$withinss)
  plot(1:15, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares", main = "K-means: distance from mean by cluster amount", cex.axis = 0.90, lwd = 2)
  abline(v=c(1:15), lty=1.5, lwd=0.5, col=336) 
  #(can also use clustergram plot for this - see A-clustergramms.R)

#K-Means (8 clusters with 10 iterations)
  clusters <- list()
  fit <- NA
  for (i in 1:10){
    print(paste("starting run", i, sep=" "))
    class.7 <- kmeans(x=cluster.data, centers=8, iter.max=1000000, nstart=1)
    fit[i] <- class.7$tot.withinss
    if (fit[i] < min(fit[1:(i-1)])){
      clusters <- class.7}
  }
  #process and show cluster size
    kfit5 <- clusters
    kfit5$size
  #extract m values
    k5_mvalues<- aggregate(cluster.data,by=list(kfit5$cluster),FUN=mean)
  #process cluster data to extract
    k5_mvalues_N <- as.data.frame(k5_mvalues[,])
  #pick a subset to create for graph 
    k5_mvalues_N <- as.data.frame(k5_mvalues[,c(1,2:6)]) #distances
    k5_mvalues_N <- as.data.frame(k5_mvalues[,c(1,7:11)]) #cars in household
    k5_mvalues_N <- as.data.frame(k5_mvalues[,c(1,12:17)]) #age
    k5_mvalues_N <- as.data.frame(k5_mvalues[,c(1,18:25)]) #Ns.sec
    k5_mvalues_N <- as.data.frame(k5_mvalues[,c(1,26:36)]) #travel to work mode
  #melt data
    k5df <- melt(k5_mvalues_N ,  id = "Group.1", variable_name = "Variables")

#save mean cluster varitaion
  #write.csv(k5df, "20150226-mean-cluster-variation.csv")
  write.csv(k5df, "20150227-mean-cluster-variation.csv")

#graph data  
  ggmeans <- ggplot(k5df, aes(Group.1,value)) + geom_line(aes(colour = Variables)) + 
    scale_x_continuous(breaks=seq(1, 7, by=1)) + theme(text = element_text(size=20))
  ggmeans + ggtitle("Mean variation within clusters")

#Append cluster-OAC
  OA_data_centriods <- pop_centriods_data_census
  OA_data_centriods$cluster <- kfit5$cluster
  write.csv(OA_data_centriods, file = "20150227-OA-final.csv")
  OA_final <- data.frame(OA_data_centriods$OA11CD, kfit5$cluster)
  #export as CSV
    write.csv(OA_final, file = "20150225-OA-final.csv")

#look at cluster data
  #list
    as.data.frame(table(kfit5$cluster))
    as.data.frame(table(OA_final$kfit5.cluster))

#read in shape file, join and save
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

