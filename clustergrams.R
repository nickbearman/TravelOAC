#recieved from Den 20150225

# download function code for clustergram function from 
# https://raw.githubusercontent.com/talgalili/R-code-snippets/master/clustergram.r

# Get Data
masterdata <- read.csv("/Users/deanriddlesden/Google Drive/Geodemographic/Master Database/masterdata.csv")

#remove first col - non numeric LSOA codes
Data <-masterdata[,-1]
#scale - this is essential to the clustergram running (doesnt matter if data is already scaled)
Data <- scale(Data)

#Single clustergram
set.seed(250)
clustergram(Data, k.range = 2:10, line.width = 0.00004)
clustergram(cluster.data, k.range = 2:15, line.width = 0.00004)

# multiple iterations (to see if there are any major changes)
par(cex.lab = 1.2, cex.main = .7)
par(mfrow = c(3,2))
for(i in 1:6) clustergram(cluster.data, k.range = 2:10 , line.width = 0.00002, add.center.points = T)



