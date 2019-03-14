library(ggplot2)
par(mfrow=c(3,1) )
par(oma=c(0,0,2,0))

##################################################################################################################################################################################
####################################################   SESSION 1   ###############################################################################################################



####################  READ DATA   ###########################################################
rm(list=ls())

setwd("C:\\Users\\MANJU VISVANATHAN\\Desktop\\Stats Project\\subject25\\session1")
list.files()

Subject_Session1_Cutting = read.table("Subject25_Cutting1.csv",header = TRUE, sep=",",stringsAsFactors = FALSE)

Subject_Session1_Surturing = read.table("Subject25_Suturing1.csv",header = TRUE, sep=",",stringsAsFactors = FALSE)

Subject_Session1_Baseline = read.table("Subject25_Baseline1.csv",header = TRUE, sep=",",stringsAsFactors = FALSE)


Max_Time_B = 0
Max_Time_C = 0
Max_Time_S = 0
wZb = 0
wZc = 0
wZs = 0
####################  CUTTING   ###########################################################

if(exists("Subject_Session1_Cutting"))
{
  Max_Time_C = as.integer(max(Subject_Session1_Cutting$Time))
  
  Average_Perspiration_Cutting  = rep(0,(Max_Time_C +1))
  
  j= 1
  counter = 0  
  Average = 0
  for(i in 1: length(Subject_Session1_Cutting$Time))
  {
    if( (j-1) != as.integer(Subject_Session1_Cutting$Time[i])){
      Average_Perspiration_Cutting[j] = Average / counter
      j =j + 1
      counter = 0
      Average = 0
    }
    if( (j-1) == as.integer(Subject_Session1_Cutting$Time[i]) ){
      Average = Average + Subject_Session1_Cutting$Perspiration[i]
      counter = counter  + 1  
    }
    
  }
  Average_Perspiration_Cutting[j] = Average / counter
  
  Average_Perspiration_Cutting <- na.omit(Average_Perspiration_Cutting)
  
  wZc = zoo(Average_Perspiration_Cutting)
}
#######################   SUTURING   ########################################################

if(exists("Subject_Session1_Surturing"))
{
  Max_Time_S = as.integer(max(Subject_Session1_Surturing$Time))
  
  Average_Perspiration_Suturing  = rep(0,(Max_Time_S +1))
  
  j= 1
  counter = 0  
  Average = 0
  for(i in 1: length(Subject_Session1_Surturing$Time))
  {
    if( (j-1) != as.integer(Subject_Session1_Surturing$Time[i])){
      Average_Perspiration_Suturing[j] = Average / counter
      j =j + 1
      counter = 0
      Average = 0
    }
    if( (j-1) == as.integer(Subject_Session1_Surturing$Time[i]) ){
      Average = Average + Subject_Session1_Surturing$Perspiration[i]
      counter = counter  + 1  
    }
    
  }
  Average_Perspiration_Suturing[j] = Average / counter
  
  Average_Perspiration_Suturing <- na.omit(Average_Perspiration_Suturing)
  
  wZs = zoo(Average_Perspiration_Suturing)
  
}
############################  BASELINE   ###################################################

if(exists("Subject_Session1_Baseline"))
{
  
  Max_Time_B = as.integer(max(Subject_Session1_Baseline$Time))
  
  Average_Perspiration_Baseline  = rep(0,(Max_Time_B +1))
  
  j= 1
  counter = 0  
  Average = 0
  for(i in 1: length(Subject_Session1_Baseline$Time))
  {
    if( (j-1) != as.integer(Subject_Session1_Baseline$Time[i])){
      Average_Perspiration_Baseline[j] = Average / counter
      j =j + 1
      counter = 0
      Average = 0
    }
    if( (j-1) == as.integer(Subject_Session1_Baseline$Time[i]) ){
      Average = Average + Subject_Session1_Baseline$Perspiration[i]
      counter = counter  + 1  
    }
    
  }
  Average_Perspiration_Baseline[j] = Average / counter
  
  Average_Perspiration_Baseline <- na.omit(Average_Perspiration_Baseline)
  
  wZb = zoo(Average_Perspiration_Baseline)
}
#######################  PLOTS    ########################################################

plot(wZb, type="l", lty=1 ,xlim = c(0,1200), ylim = c(0,0.020) ,xlab=" ", ylab= expression(paste("Perinasal Perspiration [",degree,"C"^2,"]")), main="Session 1", col="black")
lines(wZc, type="l", lty=2, col="green") 
lines(wZs, type="l", lty=3, col="red") 
#legend('topright', legend=c("Baseline", "Cutting","Suturing"), col=c("black","green", "red"), lty=1:3, cex=0.8)


##################################################################################################################################################################################
####################################################   SESSION 2   ###############################################################################################################



####################  READ DATA   ###########################################################
rm(list=ls())

setwd("C:\\Users\\MANJU VISVANATHAN\\Desktop\\Stats Project\\subject25\\session2")
list.files()

Subject_Session2_Cutting = read.table("Subject25_Cutting2.csv",header = TRUE, sep=",",stringsAsFactors = FALSE)

Subject_Session2_Surturing = read.table("Subject25_Suturing2.csv",header = TRUE, sep=",",stringsAsFactors = FALSE)

Subject_Session2_Baseline = read.table("Subject25_Baseline2.csv",header = TRUE, sep=",",stringsAsFactors = FALSE)


Max_Time_B = 0
Max_Time_C = 0
Max_Time_S = 0
wZb = 0
wZc = 0
wZs = 0
####################  CUTTING   ###########################################################

if(exists("Subject_Session2_Cutting"))
{
  Max_Time_C = as.integer(max(Subject_Session2_Cutting$Time))
  
  Average_Perspiration_Cutting  = rep(0,(Max_Time_C +1))
  
  j= 1
  counter = 0  
  Average = 0
  for(i in 1: length(Subject_Session2_Cutting$Time))
  {
    if( (j-1) != as.integer(Subject_Session2_Cutting$Time[i])){
      Average_Perspiration_Cutting[j] = Average / counter
      j =j + 1
      counter = 0
      Average = 0
    }
    if( (j-1) == as.integer(Subject_Session2_Cutting$Time[i]) ){
      Average = Average + Subject_Session2_Cutting$Perspiration[i]
      counter = counter  + 1  
    }
    
  }
  Average_Perspiration_Cutting[j] = Average / counter
  
  Average_Perspiration_Cutting <- na.omit(Average_Perspiration_Cutting)
  
  wZc = zoo(Average_Perspiration_Cutting)
}
#######################   SUTURING   ########################################################

if(exists("Subject_Session2_Surturing"))
{
  Max_Time_S = as.integer(max(Subject_Session2_Surturing$Time))
  
  Average_Perspiration_Suturing  = rep(0,(Max_Time_S +1))
  
  j= 1
  counter = 0  
  Average = 0
  for(i in 1: length(Subject_Session2_Surturing$Time))
  {
    if( (j-1) != as.integer(Subject_Session2_Surturing$Time[i])){
      Average_Perspiration_Suturing[j] = Average / counter
      j =j + 1
      counter = 0
      Average = 0
    }
    if( (j-1) == as.integer(Subject_Session2_Surturing$Time[i]) ){
      Average = Average + Subject_Session2_Surturing$Perspiration[i]
      counter = counter  + 1  
    }
    
  }
  Average_Perspiration_Suturing[j] = Average / counter
  
  Average_Perspiration_Suturing <- na.omit(Average_Perspiration_Suturing)
  
  wZs = zoo(Average_Perspiration_Suturing)
  
}
############################  BASELINE   ###################################################

if(exists("Subject_Session2_Baseline"))
{
  
  Max_Time_B = as.integer(max(Subject_Session2_Baseline$Time))
  
  Average_Perspiration_Baseline  = rep(0,(Max_Time_B +1))
  
  j= 1
  counter = 0  
  Average = 0
  for(i in 1: length(Subject_Session2_Baseline$Time))
  {
    if( (j-1) != as.integer(Subject_Session2_Baseline$Time[i])){
      Average_Perspiration_Baseline[j] = Average / counter
      j =j + 1
      counter = 0
      Average = 0
    }
    if( (j-1) == as.integer(Subject_Session2_Baseline$Time[i]) ){
      Average = Average + Subject_Session2_Baseline$Perspiration[i]
      counter = counter  + 1  
    }
    
  }
  Average_Perspiration_Baseline[j] = Average / counter
  
  Average_Perspiration_Baseline <- na.omit(Average_Perspiration_Baseline)
  
  wZb = zoo(Average_Perspiration_Baseline)
}
#######################  PLOTS    ########################################################

plot(wZb, type="l", lty=1 ,xlim = c(0,1200), ylim = c(0,0.020) ,xlab="Time in seconds", ylab= expression(paste("Perinasal Perspiration [",degree,"C"^2,"]")), main="Session 2", col="black")
lines(wZc, type="l", lty=2, col="green") 
lines(wZs, type="l", lty=3, col="red") 
#legend('topright', legend=c("Baseline", "Cutting","Suturing"), col=c("black","green", "red"), lty=1:3, cex=0.8)

###################################################################################################################


plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend('center',horiz = T, legend=c("Baseline", "Cutting","Suturing"), col=c("black","green", "red"), lty=1:3, cex=1.4)

title(main="Subject 25 : Stress Signals",outer=T)