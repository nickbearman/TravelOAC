#File A-clustergrams
#Code to create a clstergram graph to select the appropiate number of clusters for the K-means classification 
#Part of TravelOAC GitHub Repository, see Readme file for details
#Refered to in 3-calc-clusters.R

#Requirements:
  #download the function code for clustergram function from 
  #https://raw.githubusercontent.com/talgalili/R-code-snippets/master/clustergram.r
  #and run to setup clustergram() function

#read in data to Data variable

#scale - this is essential to the clustergram running (doesnt matter if data is already scaled)
  Data <- scale(Data)

#Single clustergram
  set.seed(250)
  clustergram(Data, k.range = 2:10, line.width = 0.00004)
  clustergram(cluster.data, k.range = 2:15, line.width = 0.00004)

#Multiple iterations (to see if there are any major changes)
  par(cex.lab = 1.2, cex.main = .7)
  par(mfrow = c(3,2))
  for(i in 1:6) clustergram(cluster.data, k.range = 2:10 , line.width = 0.00002, add.center.points = T)
