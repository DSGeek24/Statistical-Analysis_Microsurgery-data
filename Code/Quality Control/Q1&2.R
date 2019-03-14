setwd("C:\\Users\\MANJU VISVANATHAN\\Desktop\\Stats Project")


##################################   Histogram for Age Distribution:    ####################################################
 
   Microsurgery_Performance <- read.csv(file="MicrosurgeryPerformance.csv",header = TRUE)
  
  hist(Microsurgery_Performance$Age,main="Histogram for Age Distribution", xlab="Age", border="red", col="blue",labels=TRUE,ylim = c(0,12))


/********************************** Barplot for Gender distribution  ******************************************************/
  
setwd("C:\\Users\\MANJU VISVANATHAN\\Documents\\Project")

Microsurgery_Performance <- read.csv(file="MicrosurgeryPerformance.csv",header = TRUE)

Performance <- as.numeric(as.character(Microsurgery_Performance$Sex))

Performance <- na.omit(Performance)

Gender <- table(Performance)

xx <- barplot(Gender, main="BarPlot for Gender Distribution",names.arg = c("Male","Female"), xlab="Gender",ylab= "No. of Subjects",col=c("darkblue","red"),ylim = c(0,11))

## Add text at top of bars
text(x = xx, y = Gender, label = Gender, pos = 3, cex = 0.8, col = "red")