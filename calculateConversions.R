#Loading in all the tables
codonTableAll <- read.table("Desktop/HumanCodonDataFrame.txt",sep = "\t", header = TRUE)
codonTableSmall <- read.csv("Desktop/HumanSmall.csv",sep = "\t", header = TRUE)
codonTableMid1 <- read.csv("Desktop/HumanMid1.csv",sep = "\t", header = TRUE)
codonTableMid2 <- read.csv("Desktop/HumanMid2.csv.csv",sep = "\t", header = TRUE)
codonTableLong <- read.csv("Desktop/HumanLarge.csv.csv",sep = "\t", header = TRUE)

#changing the comma's in the tables to points.
codonTableAll$Transcription.Rate <- as.numeric(gsub(",", ".", gsub("\\.", "", codonTableAll$Transcription.Rate)))
codonTableSmall$Transcription.Rate <- as.numeric(gsub(",", ".", gsub("\\.", "", codonTableSmall$Transcription.Rate)))
codonTableMid1$Transcription.Rate <- as.numeric(gsub(",", ".", gsub("\\.", "", codonTableMid1$Transcription.Rate)))
codonTableMid2$Transcription.Rate <- as.numeric(gsub(",", ".", gsub("\\.", "", codonTableMid2$Transcription.Rate)))
codonTableLong$Transcription.Rate <- as.numeric(gsub(",", ".", gsub("\\.", "", codonTableLong$Transcription.Rate)))


#this function calculates the correlation for the given codon for each of tables with the pearson calculation
#and with the spearman calculation. It returns a list with all of the correlations.
corFun <- function(x){
  cp1 <-cor(codonTableAll$Transcription.Rate , codonTableAll[,x],method = "pearson")
  cp2 <-cor(codonTableSmall$Transcription.Rate , codonTableSmall[,x],method = "pearson")
  cp3 <-cor(codonTableMid1$Transcription.Rate , codonTableMid1[,x],method = "pearson")
  cp4 <-cor(codonTableMid2$Transcription.Rate , codonTableMid2[,x],method = "pearson")
  cp5 <-cor(codonTableLong$Transcription.Rate , codonTableLong[,x],method = "pearson")
  cs1 <-cor(codonTableAll$Transcription.Rate , codonTableAll[,x],method = "spearman")
  cs2 <-cor(codonTableSmall$Transcription.Rate , codonTableSmall[,x],method = "spearman")
  cs3 <-cor(codonTableMid1$Transcription.Rate , codonTableMid1[,x],method = "spearman")
  cs4 <-cor(codonTableMid2$Transcription.Rate , codonTableMid2[,x],method = "spearman")
  cs5 <-cor(codonTableLong$Transcription.Rate , codonTableLong[,x],method = "spearman")
  l <- list(colnames(codonTableAll)[x],cp1,cs1,cp2,cs2,cp3,cs3,cp4,cs4,cp5,cs5)
  return(l)
}
#A for loop over the columns that contain codon amounts and sends the columns to the function.
df <- rbind(corFun(4))
for (i in 5:67) {
  df <- rbind(corFun(i),df)
}
View(df)
#creates a file with the new correlation dataframe.
write.csv(df, "Desktop/ProteinCorrelationTable.csv")

#a function that creates plots with a column, a table and a name.
plotFun <- function(x, table, tname){
  
  glm <- glm(table$Transcription.Rate ~ table[,x])
  par(cex=.8)
  plot(table$Transcription.Rate ~ table[,x], xlab = colnames(table)[x], main = tname )
  abline(glm, col = "red" )
}

#creates a 5 plots for all of the 5 tables with the codon AAG
par(mfrow=c(2,3))
plotFun(29,codonTableAll, "All")
plotFun(29,codonTableSmall, "Small")
plotFun(29,codonTableMid1, "Mid1")
plotFun(29,codonTableMid2, "Mid2")
plotFun(29,codonTableLong, "Long")



