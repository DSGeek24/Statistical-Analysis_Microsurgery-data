j = c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26")
par(oma=c(0,0,2,0))
par(mfrow=c(5,5))
setwd("/Users/priscilla/Documents/statistics/Project/Methodist microsurgery with output/")
fp = "/Users/priscilla/Documents/statistics/Project/Methodist microsurgery with output/"
#############################################################################################################
i = 1
filepath = paste("subject",
                 j[i],
                 "/session1",
                 "/Subject",
                 j[i],
                 "_Cutting1_NASA.csv",
                 sep = "")
temp = paste(fp, filepath, sep = "")
Session1_Cutting <- read.csv(file=filepath,header=TRUE) 
filepath = paste("subject",
                 j[i],
                 "/session2",
                 "/Subject",
                 j[i],
                 "_Cutting2_NASA.csv",
                 sep = "")
temp = paste(fp, filepath, sep = "")
Session2_Cutting <- read.csv(file=filepath,header=TRUE) 
filepath = paste("subject",
                 j[i],
                 "/session3",
                 "/Subject",
                 j[i],
                 "_Cutting3_NASA.csv",
                 sep = "")
temp = paste(fp, filepath, sep = "")

Session3_Cutting <- read.csv(file=filepath,header=TRUE) 
filepath = paste("subject",
                 j[i],
                 "/session4",
                 "/Subject",
                 j[i],
                 "_Cutting4_NASA.csv",
                 sep = "")
temp = paste(fp, filepath, sep = "")

Session4_Cutting <- read.csv(file=filepath,header=TRUE) 
filepath = paste("subject",
                 j[i],
                 "/session5",
                 "/Subject",
                 j[i],
                 "_Cutting5_NASA.csv",
                 sep = "")
temp = paste(fp, filepath, sep = "")

Session5_Cutting <- read.csv(file=filepath,header=TRUE) 
data <- data.frame(C1=Session1_Cutting$Cutting1, C2=Session2_Cutting$Cutting2,C3=Session3_Cutting$Cutting3,C4=Session4_Cutting$Cutting4,C5=Session5_Cutting$Cutting5) 
#,row.names =Session1_Cutting$Response

data

data.mat <- as.matrix(data)[,1:5]
barplot(t(data.mat),beside=TRUE,main="Subject 1",col=c("gray","red","green","blue", "yellow"), xlab = " ")
