install.packages("gplots")
library(gplots) 
file <- read.table("C:\\Users\\Rutger\\Desktop\\CodonUsage_cDNA_Renamed_653341.txt",header=TRUE)
#a new copy of the dataframe without the first row since those will become the rownames later.
dataframe <- file[,-1]
#the rownames are assigned
rownames(dataframe) <- file[,1]
#the means for each row are calculated
codonMeans <- rowMeans(dataframe, na.rm = TRUE)
#a new dataframe is created
codonDF <- dataframe
#a new column is added called Mean and the calculated means are added to the dataframe. 
codonDF$Mean <- codonMeans

#a new dataframe is made this time by calculation the deviation of the mean for each 
#codon count
percentageDifference <- (codonDF[1:64,1:12]/codonDF[1:64,13])*100 - 100

percentageDifference <- as.matrix(percentageDifference)
#clears the image part of Rstudio so an image can be loaded otherwise errors appear.
dev.off()               # close the PNG device
#the margins are set.
par(mar=c(1,1,1,1))
#creaton of the heatmap.
heatmap.2(percentageDifference,
          main = "Codon Deviation of Mean 653341", # heat map title  
          trace="none",         # turns off trace lines inside the heat map
          dendrogram="none",     # only draw a row dendrogram
          col=redblue(75),       # use on color palette defined earlier
          margin=c(3,5),
          Colv="NA",
          Rowv="NA",
          cexRow = 0.5)            # turn off column clustering
