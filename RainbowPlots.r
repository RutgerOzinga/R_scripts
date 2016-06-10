#reads the dataframes
df <-read.csv("Downloads/Human Conversion Correlation - Sheet1.csv")
df2 <- read.csv("Downloads/Codon conversion correlation - Sheet1.csv")

#creates a fuction for the colors.
color_pallete_function <- colorRampPalette(
  colors = rainbow(21),
  space = "Lab"
)

#all of the levels of amino acid.
num_colors <- nlevels(df$Amino.Acid)
#selects the colors for each of the amino acid levels.
codon_colors <- color_pallete_function(num_colors)

#a function that creates plots with a table, the column and a title.
#the table includes a legend and two x axis one for the codon names and one for the amino acid names
barplotFunction <- function(dataframe, column, title) {
  p = barplot(dataframe[,column],
        col = codon_colors[dataframe$Amino.Acid],
        ylim = c(-0.32,0.32), 
        xlab = "Codons",
        ylab = "Correlation",
        main = title,
        sub = colnames(dataframe[column])
        
        )
  legend("topright",title = "AminoAcids",cex = 0.5,
         fill=unique(codon_colors[dataframe$Amino.Acid]),
         legend=unique(dataframe[,1]), xpd = TRUE, inset= c(-0.1,0)
         )
  axis(1,at= p, labels = dataframe[,2],cex.axis=0.4,las =2)
  axis(1,line=1,at= p, labels = dataframe[,1],cex.axis=0.4,las =2, tick = FALSE)
}
#gives the graphical parameters for the plots and the legend
par(mar=c(5.1,4.1,4.1,6.1),mfrow=c(1,1),xpd = TRUE)

#creates the plots for each of the rows with pearson data
for(x in seq(3,12, by=2)){
  #barplotFunction(df,x, "Human Lung")
  barplotFunction(df2,x,"Rat Liver")
  
}

