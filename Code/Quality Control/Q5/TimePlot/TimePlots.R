
data<-read.csv("D:/Downloads/osfstorage-archive/Methodist microsurgery with output/MicrosurgeryPerformance.csv")

library(ggplot2)
par(mfrow=c(3,3))
par(oma=c(0,0,2,0))

#Subject1

#Session 1 

cutting_time1<-data$Cutting.Time.1
cutting_time_1<-as.character(cutting_time1[1])

minutes_cut1<-vapply(strsplit(cutting_time_1,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut1<-vapply(strsplit(cutting_time_1,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut1<-as.numeric(minutes_cut1)*60+ as.numeric(seconds_cut1)

suturing_time1<-data$Suturing.Time.1
suturing_time_1<-as.character(suturing_time1[1])

minutes_sut1<-vapply(strsplit(suturing_time_1,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut1<-vapply(strsplit(suturing_time_1,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut1<-as.numeric(minutes_sut1)*60+ as.numeric(seconds_sut1)

#Session 2

cutting_time2<-data$Cutting.Time.2
cutting_time_2<-as.character(cutting_time2[1])

minutes_cut2<-vapply(strsplit(cutting_time_2,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut2<-vapply(strsplit(cutting_time_2,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut2<-as.numeric(minutes_cut2)*60+ as.numeric(seconds_cut2)

suturing_time2<-data$Suturing.Time.2
suturing_time_2<-as.character(suturing_time2[1])

minutes_sut2<-vapply(strsplit(suturing_time_2,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut2<-vapply(strsplit(suturing_time_2,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut2<-as.numeric(minutes_sut2)*60+ as.numeric(seconds_sut2)

#Session 3

cutting_time3<-data$Cutting.Time.3
cutting_time_3<-as.character(cutting_time3[1])

minutes_cut3<-vapply(strsplit(cutting_time_3,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut3<-vapply(strsplit(cutting_time_3,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut3<-as.numeric(minutes_cut3)*60+ as.numeric(seconds_cut3)

suturing_time3<-data$Suturing.Time.3
suturing_time_3<-as.character(suturing_time3[1])

minutes_sut3<-vapply(strsplit(suturing_time_3,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut3<-vapply(strsplit(suturing_time_3,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut3<-as.numeric(minutes_sut3)*60+ as.numeric(seconds_sut3)

#Session 4

cutting_time4<-data$Cutting.Time.4
cutting_time_4<-as.character(cutting_time4[1])

minutes_cut4<-vapply(strsplit(cutting_time_4,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut4<-vapply(strsplit(cutting_time_4,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut4<-as.numeric(minutes_cut4)*60+ as.numeric(seconds_cut4)

suturing_time4<-data$Suturing.Time.4
suturing_time_4<-as.character(suturing_time4[1])

minutes_sut4<-vapply(strsplit(suturing_time_4,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut4<-vapply(strsplit(suturing_time_4,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut4<-as.numeric(minutes_sut4)*60+ as.numeric(seconds_sut4)

#Session 5

cutting_time5<-data$Cutting.Time.5
cutting_time_5<-as.character(cutting_time5[1])

minutes_cut5<-vapply(strsplit(cutting_time_5,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut5<-vapply(strsplit(cutting_time_5,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut5<-as.numeric(minutes_cut5)*60+ as.numeric(seconds_cut5)

suturing_time5<-data$Suturing.Time.5
suturing_time_5<-as.character(suturing_time5[1])

minutes_sut5<-vapply(strsplit(suturing_time_5,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut5<-vapply(strsplit(suturing_time_5,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut5<-as.numeric(minutes_sut5)*60+ as.numeric(seconds_sut5)

Cutting_time = c(total_seconds_cut1,total_seconds_cut2,total_seconds_cut3,total_seconds_cut4,total_seconds_cut5)

Suturing_time = c(total_seconds_sut1,total_seconds_sut2,total_seconds_sut3,total_seconds_sut4,total_seconds_sut5)

data <- data.frame(CuttingTime=Cutting_time, SuturingTime=Suturing_time,row.names = c(1,2,3,4,5)) 

data

task.mat <- as.matrix(data)[,1:2]

barplot(t(task.mat),beside=TRUE,main="Time Bar Plot-Cutting Vs Suturing(Subject1)",col=c("gray","blue"),ylim=c(0,1300),xlab="Session",ylab="Time (secs)")

#Subject2

#Session 1 

cutting_time1<-data$Cutting.Time.1
cutting_time_1<-as.character(cutting_time1[2])

minutes_cut1<-vapply(strsplit(cutting_time_1,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut1<-vapply(strsplit(cutting_time_1,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut1<-as.numeric(minutes_cut1)*60+ as.numeric(seconds_cut1)

suturing_time1<-data$Suturing.Time.1
suturing_time_1<-as.character(suturing_time1[2])

minutes_sut1<-vapply(strsplit(suturing_time_1,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut1<-vapply(strsplit(suturing_time_1,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut1<-as.numeric(minutes_sut1)*60+ as.numeric(seconds_sut1)

#Session 2

cutting_time2<-data$Cutting.Time.2
cutting_time_2<-as.character(cutting_time2[2])

minutes_cut2<-vapply(strsplit(cutting_time_2,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut2<-vapply(strsplit(cutting_time_2,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut2<-as.numeric(minutes_cut2)*60+ as.numeric(seconds_cut2)

suturing_time2<-data$Suturing.Time.2
suturing_time_2<-as.character(suturing_time2[2])

minutes_sut2<-vapply(strsplit(suturing_time_2,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut2<-vapply(strsplit(suturing_time_2,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut2<-as.numeric(minutes_sut2)*60+ as.numeric(seconds_sut2)

#Session 3

cutting_time3<-data$Cutting.Time.3
cutting_time_3<-as.character(cutting_time3[2])

minutes_cut3<-vapply(strsplit(cutting_time_3,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut3<-vapply(strsplit(cutting_time_3,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut3<-as.numeric(minutes_cut3)*60+ as.numeric(seconds_cut3)

suturing_time3<-data$Suturing.Time.3
suturing_time_3<-as.character(suturing_time3[2])

minutes_sut3<-vapply(strsplit(suturing_time_3,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut3<-vapply(strsplit(suturing_time_3,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut3<-as.numeric(minutes_sut3)*60+ as.numeric(seconds_sut3)

#Session 4

cutting_time4<-data$Cutting.Time.4
cutting_time_4<-as.character(cutting_time4[2])

minutes_cut4<-vapply(strsplit(cutting_time_4,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut4<-vapply(strsplit(cutting_time_4,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut4<-as.numeric(minutes_cut4)*60+ as.numeric(seconds_cut4)

suturing_time4<-data$Suturing.Time.4
suturing_time_4<-as.character(suturing_time4[2])

minutes_sut4<-vapply(strsplit(suturing_time_4,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut4<-vapply(strsplit(suturing_time_4,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut4<-as.numeric(minutes_sut4)*60+ as.numeric(seconds_sut4)

#Session 5

cutting_time5<-data$Cutting.Time.5
cutting_time_5<-as.character(cutting_time5[2])

minutes_cut5<-vapply(strsplit(cutting_time_5,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut5<-vapply(strsplit(cutting_time_5,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut5<-as.numeric(minutes_cut5)*60+ as.numeric(seconds_cut5)

suturing_time5<-data$Suturing.Time.5
suturing_time_5<-as.character(suturing_time5[2])

minutes_sut5<-vapply(strsplit(suturing_time_5,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut5<-vapply(strsplit(suturing_time_5,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut5<-as.numeric(minutes_sut5)*60+ as.numeric(seconds_sut5)

Cutting_time = c(total_seconds_cut1,total_seconds_cut2,total_seconds_cut3,total_seconds_cut4,total_seconds_cut5)

Suturing_time = c(total_seconds_sut1,total_seconds_sut2,total_seconds_sut3,total_seconds_sut4,total_seconds_sut5)

data <- data.frame(CuttingTime=Cutting_time, SuturingTime=Suturing_time,row.names = c(1,2,3,4,5)) 

data

task.mat <- as.matrix(data)[,1:2]

barplot(t(task.mat),beside=TRUE,main="Subject2",col=c("gray","blue"),ylim=c(0,1300))

#Subject3

#Session 1 

cutting_time1<-data$Cutting.Time.1
cutting_time_1<-as.character(cutting_time1[3])

minutes_cut1<-vapply(strsplit(cutting_time_1,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut1<-vapply(strsplit(cutting_time_1,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut1<-as.numeric(minutes_cut1)*60+ as.numeric(seconds_cut1)

suturing_time1<-data$Suturing.Time.1
suturing_time_1<-as.character(suturing_time1[3])

minutes_sut1<-vapply(strsplit(suturing_time_1,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut1<-vapply(strsplit(suturing_time_1,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut1<-as.numeric(minutes_sut1)*60+ as.numeric(seconds_sut1)

#Session 2

cutting_time2<-data$Cutting.Time.2
cutting_time_2<-as.character(cutting_time2[3])

minutes_cut2<-vapply(strsplit(cutting_time_2,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut2<-vapply(strsplit(cutting_time_2,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut2<-as.numeric(minutes_cut2)*60+ as.numeric(seconds_cut2)

suturing_time2<-data$Suturing.Time.2
suturing_time_2<-as.character(suturing_time2[3])

minutes_sut2<-vapply(strsplit(suturing_time_2,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut2<-vapply(strsplit(suturing_time_2,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut2<-as.numeric(minutes_sut2)*60+ as.numeric(seconds_sut2)

#Session 3

cutting_time3<-data$Cutting.Time.3
cutting_time_3<-as.character(cutting_time3[3])

minutes_cut3<-vapply(strsplit(cutting_time_3,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut3<-vapply(strsplit(cutting_time_3,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut3<-as.numeric(minutes_cut3)*60+ as.numeric(seconds_cut3)

suturing_time3<-data$Suturing.Time.3
suturing_time_3<-as.character(suturing_time3[3])

minutes_sut3<-vapply(strsplit(suturing_time_3,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut3<-vapply(strsplit(suturing_time_3,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut3<-as.numeric(minutes_sut3)*60+ as.numeric(seconds_sut3)

#Session 4

cutting_time4<-data$Cutting.Time.4
cutting_time_4<-as.character(cutting_time4[3])

minutes_cut4<-vapply(strsplit(cutting_time_4,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut4<-vapply(strsplit(cutting_time_4,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut4<-as.numeric(minutes_cut4)*60+ as.numeric(seconds_cut4)

suturing_time4<-data$Suturing.Time.4
suturing_time_4<-as.character(suturing_time4[3])

minutes_sut4<-vapply(strsplit(suturing_time_4,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut4<-vapply(strsplit(suturing_time_4,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut4<-as.numeric(minutes_sut4)*60+ as.numeric(seconds_sut4)

#Session 5

cutting_time5<-data$Cutting.Time.5
cutting_time_5<-as.character(cutting_time5[3])

minutes_cut5<-vapply(strsplit(cutting_time_5,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut5<-vapply(strsplit(cutting_time_5,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut5<-as.numeric(minutes_cut5)*60+ as.numeric(seconds_cut5)

suturing_time5<-data$Suturing.Time.5
suturing_time_5<-as.character(suturing_time5[3])

minutes_sut5<-vapply(strsplit(suturing_time_5,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut5<-vapply(strsplit(suturing_time_5,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut5<-as.numeric(minutes_sut5)*60+ as.numeric(seconds_sut5)

Cutting_time = c(total_seconds_cut1,total_seconds_cut2,total_seconds_cut3,total_seconds_cut4,total_seconds_cut5)

Suturing_time = c(total_seconds_sut1,total_seconds_sut2,total_seconds_sut3,total_seconds_sut4,total_seconds_sut5)

data <- data.frame(CuttingTime=Cutting_time, SuturingTime=Suturing_time,row.names = c(1,2,3,4,5)) 

data

task.mat <- as.matrix(data)[,1:2]

barplot(t(task.mat),beside=TRUE,main="Subject3",col=c("gray","blue"),ylim=c(0,1300))

#Subject4

#Session 1 

cutting_time1<-data$Cutting.Time.1
cutting_time_1<-as.character(cutting_time1[4])

minutes_cut1<-vapply(strsplit(cutting_time_1,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut1<-vapply(strsplit(cutting_time_1,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut1<-as.numeric(minutes_cut1)*60+ as.numeric(seconds_cut1)

suturing_time1<-data$Suturing.Time.1
suturing_time_1<-as.character(suturing_time1[4])

minutes_sut1<-vapply(strsplit(suturing_time_1,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut1<-vapply(strsplit(suturing_time_1,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut1<-as.numeric(minutes_sut1)*60+ as.numeric(seconds_sut1)

#Session 2

cutting_time2<-data$Cutting.Time.2
cutting_time_2<-as.character(cutting_time2[4])

minutes_cut2<-vapply(strsplit(cutting_time_2,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut2<-vapply(strsplit(cutting_time_2,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut2<-as.numeric(minutes_cut2)*60+ as.numeric(seconds_cut2)

suturing_time2<-data$Suturing.Time.2
suturing_time_2<-as.character(suturing_time2[4])

minutes_sut2<-vapply(strsplit(suturing_time_2,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut2<-vapply(strsplit(suturing_time_2,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut2<-as.numeric(minutes_sut2)*60+ as.numeric(seconds_sut2)

#Session 3

cutting_time3<-data$Cutting.Time.3
cutting_time_3<-as.character(cutting_time3[4])

minutes_cut3<-vapply(strsplit(cutting_time_3,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut3<-vapply(strsplit(cutting_time_3,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut3<-as.numeric(minutes_cut3)*60+ as.numeric(seconds_cut3)

suturing_time3<-data$Suturing.Time.3
suturing_time_3<-as.character(suturing_time3[4])

minutes_sut3<-vapply(strsplit(suturing_time_3,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut3<-vapply(strsplit(suturing_time_3,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut3<-as.numeric(minutes_sut3)*60+ as.numeric(seconds_sut3)

#Session 4

cutting_time4<-data$Cutting.Time.4
cutting_time_4<-as.character(cutting_time4[4])

minutes_cut4<-vapply(strsplit(cutting_time_4,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut4<-vapply(strsplit(cutting_time_4,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut4<-as.numeric(minutes_cut4)*60+ as.numeric(seconds_cut4)

suturing_time4<-data$Suturing.Time.4
suturing_time_4<-as.character(suturing_time4[4])

minutes_sut4<-vapply(strsplit(suturing_time_4,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut4<-vapply(strsplit(suturing_time_4,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut4<-as.numeric(minutes_sut4)*60+ as.numeric(seconds_sut4)

#Session 5

cutting_time5<-data$Cutting.Time.5
cutting_time_5<-as.character(cutting_time5[4])

minutes_cut5<-vapply(strsplit(cutting_time_5,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut5<-vapply(strsplit(cutting_time_5,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut5<-as.numeric(minutes_cut5)*60+ as.numeric(seconds_cut5)

suturing_time5<-data$Suturing.Time.5
suturing_time_5<-as.character(suturing_time5[4])

minutes_sut5<-vapply(strsplit(suturing_time_5,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut5<-vapply(strsplit(suturing_time_5,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut5<-as.numeric(minutes_sut5)*60+ as.numeric(seconds_sut5)

Cutting_time = c(total_seconds_cut1,total_seconds_cut2,total_seconds_cut3,total_seconds_cut4,total_seconds_cut5)

Suturing_time = c(total_seconds_sut1,total_seconds_sut2,total_seconds_sut3,total_seconds_sut4,total_seconds_sut5)

data <- data.frame(CuttingTime=Cutting_time, SuturingTime=Suturing_time,row.names = c(1,2,3,4,5)) 

data

task.mat <- as.matrix(data)[,1:2]

barplot(t(task.mat),beside=TRUE,main="Subject4",col=c("gray","blue"),ylim=c(0,1300),ylab="Time (secs)")

#Subject5

#Session 1 

cutting_time1<-data$Cutting.Time.1
cutting_time_1<-as.character(cutting_time1[5])

minutes_cut1<-vapply(strsplit(cutting_time_1,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut1<-vapply(strsplit(cutting_time_1,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut1<-as.numeric(minutes_cut1)*60+ as.numeric(seconds_cut1)

suturing_time1<-data$Suturing.Time.1
suturing_time_1<-as.character(suturing_time1[5])

minutes_sut1<-vapply(strsplit(suturing_time_1,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut1<-vapply(strsplit(suturing_time_1,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut1<-as.numeric(minutes_sut1)*60+ as.numeric(seconds_sut1)

#Session 2

cutting_time2<-data$Cutting.Time.2
cutting_time_2<-as.character(cutting_time2[5])

minutes_cut2<-vapply(strsplit(cutting_time_2,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut2<-vapply(strsplit(cutting_time_2,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut2<-as.numeric(minutes_cut2)*60+ as.numeric(seconds_cut2)

suturing_time2<-data$Suturing.Time.2
suturing_time_2<-as.character(suturing_time2[5])

minutes_sut2<-vapply(strsplit(suturing_time_2,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut2<-vapply(strsplit(suturing_time_2,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut2<-as.numeric(minutes_sut2)*60+ as.numeric(seconds_sut2)

#Session 3

cutting_time3<-data$Cutting.Time.3
cutting_time_3<-as.character(cutting_time3[5])

minutes_cut3<-vapply(strsplit(cutting_time_3,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut3<-vapply(strsplit(cutting_time_3,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut3<-as.numeric(minutes_cut3)*60+ as.numeric(seconds_cut3)

suturing_time3<-data$Suturing.Time.3
suturing_time_3<-as.character(suturing_time3[5])

minutes_sut3<-vapply(strsplit(suturing_time_3,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut3<-vapply(strsplit(suturing_time_3,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut3<-as.numeric(minutes_sut3)*60+ as.numeric(seconds_sut3)

#Session 4

cutting_time4<-data$Cutting.Time.4
cutting_time_4<-as.character(cutting_time4[5])

minutes_cut4<-vapply(strsplit(cutting_time_4,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut4<-vapply(strsplit(cutting_time_4,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut4<-as.numeric(minutes_cut4)*60+ as.numeric(seconds_cut4)

suturing_time4<-data$Suturing.Time.4
suturing_time_4<-as.character(suturing_time4[5])

minutes_sut4<-vapply(strsplit(suturing_time_4,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut4<-vapply(strsplit(suturing_time_4,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut4<-as.numeric(minutes_sut4)*60+ as.numeric(seconds_sut4)

#Session 5

cutting_time5<-data$Cutting.Time.5
cutting_time_5<-as.character(cutting_time5[5])

minutes_cut5<-vapply(strsplit(cutting_time_5,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut5<-vapply(strsplit(cutting_time_5,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut5<-as.numeric(minutes_cut5)*60+ as.numeric(seconds_cut5)

suturing_time5<-data$Suturing.Time.5
suturing_time_5<-as.character(suturing_time5[5])

minutes_sut5<-vapply(strsplit(suturing_time_5,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut5<-vapply(strsplit(suturing_time_5,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut5<-as.numeric(minutes_sut5)*60+ as.numeric(seconds_sut5)

Cutting_time = c(total_seconds_cut1,total_seconds_cut2,total_seconds_cut3,total_seconds_cut4,total_seconds_cut5)

Suturing_time = c(total_seconds_sut1,total_seconds_sut2,total_seconds_sut3,total_seconds_sut4,total_seconds_sut5)

data <- data.frame(CuttingTime=Cutting_time, SuturingTime=Suturing_time,row.names = c(1,2,3,4,5)) 

data

task.mat <- as.matrix(data)[,1:2]

barplot(t(task.mat),beside=TRUE,main="Subject7",col=c("gray","blue"),ylim=c(0,1300))

#Subject6

#Session 1 

cutting_time1<-data$Cutting.Time.1
cutting_time_1<-as.character(cutting_time1[6])

minutes_cut1<-vapply(strsplit(cutting_time_1,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut1<-vapply(strsplit(cutting_time_1,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut1<-as.numeric(minutes_cut1)*60+ as.numeric(seconds_cut1)

suturing_time1<-data$Suturing.Time.1
suturing_time_1<-as.character(suturing_time1[6])

minutes_sut1<-vapply(strsplit(suturing_time_1,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut1<-vapply(strsplit(suturing_time_1,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut1<-as.numeric(minutes_sut1)*60+ as.numeric(seconds_sut1)

#Session 2

cutting_time2<-data$Cutting.Time.2
cutting_time_2<-as.character(cutting_time2[6])

minutes_cut2<-vapply(strsplit(cutting_time_2,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut2<-vapply(strsplit(cutting_time_2,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut2<-as.numeric(minutes_cut2)*60+ as.numeric(seconds_cut2)

suturing_time2<-data$Suturing.Time.2
suturing_time_2<-as.character(suturing_time2[6])

minutes_sut2<-vapply(strsplit(suturing_time_2,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut2<-vapply(strsplit(suturing_time_2,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut2<-as.numeric(minutes_sut2)*60+ as.numeric(seconds_sut2)

#Session 3

cutting_time3<-data$Cutting.Time.3
cutting_time_3<-as.character(cutting_time3[6])

minutes_cut3<-vapply(strsplit(cutting_time_3,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut3<-vapply(strsplit(cutting_time_3,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut3<-as.numeric(minutes_cut3)*60+ as.numeric(seconds_cut3)

suturing_time3<-data$Suturing.Time.3
suturing_time_3<-as.character(suturing_time3[6])

minutes_sut3<-vapply(strsplit(suturing_time_3,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut3<-vapply(strsplit(suturing_time_3,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut3<-as.numeric(minutes_sut3)*60+ as.numeric(seconds_sut3)

#Session 4

cutting_time4<-data$Cutting.Time.4
cutting_time_4<-as.character(cutting_time4[6])

minutes_cut4<-vapply(strsplit(cutting_time_4,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut4<-vapply(strsplit(cutting_time_4,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut4<-as.numeric(minutes_cut4)*60+ as.numeric(seconds_cut4)

suturing_time4<-data$Suturing.Time.4
suturing_time_4<-as.character(suturing_time4[6])

minutes_sut4<-vapply(strsplit(suturing_time_4,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut4<-vapply(strsplit(suturing_time_4,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut4<-as.numeric(minutes_sut4)*60+ as.numeric(seconds_sut4)

#Session 5

cutting_time5<-data$Cutting.Time.5
cutting_time_5<-as.character(cutting_time5[6])

minutes_cut5<-vapply(strsplit(cutting_time_5,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut5<-vapply(strsplit(cutting_time_5,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut5<-as.numeric(minutes_cut5)*60+ as.numeric(seconds_cut5)

suturing_time5<-data$Suturing.Time.5
suturing_time_5<-as.character(suturing_time5[6])

minutes_sut5<-vapply(strsplit(suturing_time_5,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut5<-vapply(strsplit(suturing_time_5,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut5<-as.numeric(minutes_sut5)*60+ as.numeric(seconds_sut5)

Cutting_time = c(total_seconds_cut1,total_seconds_cut2,total_seconds_cut3,total_seconds_cut4,total_seconds_cut5)

Suturing_time = c(total_seconds_sut1,total_seconds_sut2,total_seconds_sut3,total_seconds_sut4,total_seconds_sut5)

data <- data.frame(CuttingTime=Cutting_time, SuturingTime=Suturing_time,row.names = c(1,2,3,4,5)) 

data

task.mat <- as.matrix(data)[,1:2]

barplot(t(task.mat),beside=TRUE,main="Subject8",col=c("gray","blue"),ylim=c(0,1300))

#Subject7

#Session 1 

cutting_time1<-data$Cutting.Time.1
cutting_time_1<-as.character(cutting_time1[7])

minutes_cut1<-vapply(strsplit(cutting_time_1,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut1<-vapply(strsplit(cutting_time_1,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut1<-as.numeric(minutes_cut1)*60+ as.numeric(seconds_cut1)

suturing_time1<-data$Suturing.Time.1
suturing_time_1<-as.character(suturing_time1[7])

minutes_sut1<-vapply(strsplit(suturing_time_1,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut1<-vapply(strsplit(suturing_time_1,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut1<-as.numeric(minutes_sut1)*60+ as.numeric(seconds_sut1)

#Session 2

cutting_time2<-data$Cutting.Time.2
cutting_time_2<-as.character(cutting_time2[7])

minutes_cut2<-vapply(strsplit(cutting_time_2,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut2<-vapply(strsplit(cutting_time_2,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut2<-as.numeric(minutes_cut2)*60+ as.numeric(seconds_cut2)

suturing_time2<-data$Suturing.Time.2
suturing_time_2<-as.character(suturing_time2[7])

minutes_sut2<-vapply(strsplit(suturing_time_2,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut2<-vapply(strsplit(suturing_time_2,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut2<-as.numeric(minutes_sut2)*60+ as.numeric(seconds_sut2)

#Session 3

cutting_time3<-data$Cutting.Time.3
cutting_time_3<-as.character(cutting_time3[7])

minutes_cut3<-vapply(strsplit(cutting_time_3,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut3<-vapply(strsplit(cutting_time_3,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut3<-as.numeric(minutes_cut3)*60+ as.numeric(seconds_cut3)

suturing_time3<-data$Suturing.Time.3
suturing_time_3<-as.character(suturing_time3[7])

minutes_sut3<-vapply(strsplit(suturing_time_3,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut3<-vapply(strsplit(suturing_time_3,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut3<-as.numeric(minutes_sut3)*60+ as.numeric(seconds_sut3)

#Session 4

cutting_time4<-data$Cutting.Time.4
cutting_time_4<-as.character(cutting_time4[7])

minutes_cut4<-vapply(strsplit(cutting_time_4,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut4<-vapply(strsplit(cutting_time_4,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut4<-as.numeric(minutes_cut4)*60+ as.numeric(seconds_cut4)

suturing_time4<-data$Suturing.Time.4
suturing_time_4<-as.character(suturing_time4[7])

minutes_sut4<-vapply(strsplit(suturing_time_4,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut4<-vapply(strsplit(suturing_time_4,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut4<-as.numeric(minutes_sut4)*60+ as.numeric(seconds_sut4)

#Session 5

cutting_time5<-data$Cutting.Time.5
cutting_time_5<-as.character(cutting_time5[7])

minutes_cut5<-vapply(strsplit(cutting_time_5,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut5<-vapply(strsplit(cutting_time_5,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut5<-as.numeric(minutes_cut5)*60+ as.numeric(seconds_cut5)

suturing_time5<-data$Suturing.Time.5
suturing_time_5<-as.character(suturing_time5[7])

minutes_sut5<-vapply(strsplit(suturing_time_5,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut5<-vapply(strsplit(suturing_time_5,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut5<-as.numeric(minutes_sut5)*60+ as.numeric(seconds_sut5)

Cutting_time = c(total_seconds_cut1,total_seconds_cut2,total_seconds_cut3,total_seconds_cut4,total_seconds_cut5)

Suturing_time = c(total_seconds_sut1,total_seconds_sut2,total_seconds_sut3,total_seconds_sut4,total_seconds_sut5)

data <- data.frame(CuttingTime=Cutting_time, SuturingTime=Suturing_time,row.names = c(1,2,3,4,5)) 

data

task.mat <- as.matrix(data)[,1:2]

barplot(t(task.mat),beside=TRUE,main="Subject10",col=c("gray","blue"),ylim=c(0,1300),ylab="Time (secs)",xlab="Session")

#Subject8

#Session 1 

cutting_time1<-data$Cutting.Time.1
cutting_time_1<-as.character(cutting_time1[8])

minutes_cut1<-vapply(strsplit(cutting_time_1,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut1<-vapply(strsplit(cutting_time_1,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut1<-as.numeric(minutes_cut1)*60+ as.numeric(seconds_cut1)

suturing_time1<-data$Suturing.Time.1
suturing_time_1<-as.character(suturing_time1[8])

minutes_sut1<-vapply(strsplit(suturing_time_1,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut1<-vapply(strsplit(suturing_time_1,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut1<-as.numeric(minutes_sut1)*60+ as.numeric(seconds_sut1)

#Session 2

cutting_time2<-data$Cutting.Time.2
cutting_time_2<-as.character(cutting_time2[8])

minutes_cut2<-vapply(strsplit(cutting_time_2,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut2<-vapply(strsplit(cutting_time_2,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut2<-as.numeric(minutes_cut2)*60+ as.numeric(seconds_cut2)

suturing_time2<-data$Suturing.Time.2
suturing_time_2<-as.character(suturing_time2[8])

minutes_sut2<-vapply(strsplit(suturing_time_2,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut2<-vapply(strsplit(suturing_time_2,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut2<-as.numeric(minutes_sut2)*60+ as.numeric(seconds_sut2)

#Session 3

cutting_time3<-data$Cutting.Time.3
cutting_time_3<-as.character(cutting_time3[8])

minutes_cut3<-vapply(strsplit(cutting_time_3,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut3<-vapply(strsplit(cutting_time_3,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut3<-as.numeric(minutes_cut3)*60+ as.numeric(seconds_cut3)

suturing_time3<-data$Suturing.Time.3
suturing_time_3<-as.character(suturing_time3[8])

minutes_sut3<-vapply(strsplit(suturing_time_3,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut3<-vapply(strsplit(suturing_time_3,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut3<-as.numeric(minutes_sut3)*60+ as.numeric(seconds_sut3)

#Session 4

cutting_time4<-data$Cutting.Time.4
cutting_time_4<-as.character(cutting_time4[8])

minutes_cut4<-vapply(strsplit(cutting_time_4,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut4<-vapply(strsplit(cutting_time_4,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut4<-as.numeric(minutes_cut4)*60+ as.numeric(seconds_cut4)

suturing_time4<-data$Suturing.Time.4
suturing_time_4<-as.character(suturing_time4[8])

minutes_sut4<-vapply(strsplit(suturing_time_4,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut4<-vapply(strsplit(suturing_time_4,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut4<-as.numeric(minutes_sut4)*60+ as.numeric(seconds_sut4)

#Session 5

cutting_time5<-data$Cutting.Time.5
cutting_time_5<-as.character(cutting_time5[8])

minutes_cut5<-vapply(strsplit(cutting_time_5,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut5<-vapply(strsplit(cutting_time_5,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut5<-as.numeric(minutes_cut5)*60+ as.numeric(seconds_cut5)

suturing_time5<-data$Suturing.Time.5
suturing_time_5<-as.character(suturing_time5[8])

minutes_sut5<-vapply(strsplit(suturing_time_5,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut5<-vapply(strsplit(suturing_time_5,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut5<-as.numeric(minutes_sut5)*60+ as.numeric(seconds_sut5)

Cutting_time = c(total_seconds_cut1,total_seconds_cut2,total_seconds_cut3,total_seconds_cut4,total_seconds_cut5)

Suturing_time = c(total_seconds_sut1,total_seconds_sut2,total_seconds_sut3,total_seconds_sut4,total_seconds_sut5)

data <- data.frame(CuttingTime=Cutting_time, SuturingTime=Suturing_time,row.names = c(1,2,3,4,5)) 

data

task.mat <- as.matrix(data)[,1:2]

barplot(t(task.mat),beside=TRUE,main="Subject11",col=c("gray","blue"),ylim=c(0,1300),ylab="Time (secs)",xlab="Session")

#Subject9

#Session 1 

cutting_time1<-data$Cutting.Time.1
cutting_time_1<-as.character(cutting_time1[9])

minutes_cut1<-vapply(strsplit(cutting_time_1,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut1<-vapply(strsplit(cutting_time_1,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut1<-as.numeric(minutes_cut1)*60+ as.numeric(seconds_cut1)

suturing_time1<-data$Suturing.Time.1
suturing_time_1<-as.character(suturing_time1[9])

minutes_sut1<-vapply(strsplit(suturing_time_1,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut1<-vapply(strsplit(suturing_time_1,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut1<-as.numeric(minutes_sut1)*60+ as.numeric(seconds_sut1)

#Session 2

cutting_time2<-data$Cutting.Time.2
cutting_time_2<-as.character(cutting_time2[9])

minutes_cut2<-vapply(strsplit(cutting_time_2,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut2<-vapply(strsplit(cutting_time_2,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut2<-as.numeric(minutes_cut2)*60+ as.numeric(seconds_cut2)

suturing_time2<-data$Suturing.Time.2
suturing_time_2<-as.character(suturing_time2[9])

minutes_sut2<-vapply(strsplit(suturing_time_2,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut2<-vapply(strsplit(suturing_time_2,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut2<-as.numeric(minutes_sut2)*60+ as.numeric(seconds_sut2)

#Session 3

cutting_time3<-data$Cutting.Time.3
cutting_time_3<-as.character(cutting_time3[9])

minutes_cut3<-vapply(strsplit(cutting_time_3,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut3<-vapply(strsplit(cutting_time_3,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut3<-as.numeric(minutes_cut3)*60+ as.numeric(seconds_cut3)

suturing_time3<-data$Suturing.Time.3
suturing_time_3<-as.character(suturing_time3[9])

minutes_sut3<-vapply(strsplit(suturing_time_3,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut3<-vapply(strsplit(suturing_time_3,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut3<-as.numeric(minutes_sut3)*60+ as.numeric(seconds_sut3)

#Session 4

cutting_time4<-data$Cutting.Time.4
cutting_time_4<-as.character(cutting_time4[9])

minutes_cut4<-vapply(strsplit(cutting_time_4,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut4<-vapply(strsplit(cutting_time_4,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut4<-as.numeric(minutes_cut4)*60+ as.numeric(seconds_cut4)

suturing_time4<-data$Suturing.Time.4
suturing_time_4<-as.character(suturing_time4[9])

minutes_sut4<-vapply(strsplit(suturing_time_4,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut4<-vapply(strsplit(suturing_time_4,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut4<-as.numeric(minutes_sut4)*60+ as.numeric(seconds_sut4)

#Session 5

cutting_time5<-data$Cutting.Time.5
cutting_time_5<-as.character(cutting_time5[9])

minutes_cut5<-vapply(strsplit(cutting_time_5,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut5<-vapply(strsplit(cutting_time_5,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut5<-as.numeric(minutes_cut5)*60+ as.numeric(seconds_cut5)

suturing_time5<-data$Suturing.Time.5
suturing_time_5<-as.character(suturing_time5[9])

minutes_sut5<-vapply(strsplit(suturing_time_5,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut5<-vapply(strsplit(suturing_time_5,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut5<-as.numeric(minutes_sut5)*60+ as.numeric(seconds_sut5)

Cutting_time = c(total_seconds_cut1,total_seconds_cut2,total_seconds_cut3,total_seconds_cut4,total_seconds_cut5)

Suturing_time = c(total_seconds_sut1,total_seconds_sut2,total_seconds_sut3,total_seconds_sut4,total_seconds_sut5)

data <- data.frame(CuttingTime=Cutting_time, SuturingTime=Suturing_time,row.names = c(1,2,3,4,5)) 

data

task.mat <- as.matrix(data)[,1:2]

barplot(t(task.mat),beside=TRUE,main="Subject12",col=c("gray","blue"),ylim=c(0,1300),ylab="Time (secs)",xlab="Session")

title(main="Time Bar Plot-Cutting Vs Suturing",outer=T,xlab="Session",ylab="Time(secs)")

##################################################################################################################]
#############Batch 2##########################################################################################
library(ggplot2)
par(mfrow=c(2,3))
par(oma=c(0,0,2,0))

#Subject10

#Session 1 

cutting_time1<-data$Cutting.Time.1
cutting_time_1<-as.character(cutting_time1[10])

minutes_cut1<-vapply(strsplit(cutting_time_1,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut1<-vapply(strsplit(cutting_time_1,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut1<-as.numeric(minutes_cut1)*60+ as.numeric(seconds_cut1)

suturing_time1<-data$Suturing.Time.1
suturing_time_1<-as.character(suturing_time1[10])

minutes_sut1<-vapply(strsplit(suturing_time_1,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut1<-vapply(strsplit(suturing_time_1,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut1<-as.numeric(minutes_sut1)*60+ as.numeric(seconds_sut1)

#Session 2

cutting_time2<-data$Cutting.Time.2
cutting_time_2<-as.character(cutting_time2[10])

minutes_cut2<-vapply(strsplit(cutting_time_2,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut2<-vapply(strsplit(cutting_time_2,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut2<-as.numeric(minutes_cut2)*60+ as.numeric(seconds_cut2)

suturing_time2<-data$Suturing.Time.2
suturing_time_2<-as.character(suturing_time2[10])

minutes_sut2<-vapply(strsplit(suturing_time_2,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut2<-vapply(strsplit(suturing_time_2,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut2<-as.numeric(minutes_sut2)*60+ as.numeric(seconds_sut2)

#Session 3

cutting_time3<-data$Cutting.Time.3
cutting_time_3<-as.character(cutting_time3[10])

minutes_cut3<-vapply(strsplit(cutting_time_3,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut3<-vapply(strsplit(cutting_time_3,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut3<-as.numeric(minutes_cut3)*60+ as.numeric(seconds_cut3)

suturing_time3<-data$Suturing.Time.3
suturing_time_3<-as.character(suturing_time3[10])

minutes_sut3<-vapply(strsplit(suturing_time_3,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut3<-vapply(strsplit(suturing_time_3,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut3<-as.numeric(minutes_sut3)*60+ as.numeric(seconds_sut3)

#Session 4

cutting_time4<-data$Cutting.Time.4
cutting_time_4<-as.character(cutting_time4[10])

minutes_cut4<-vapply(strsplit(cutting_time_4,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut4<-vapply(strsplit(cutting_time_4,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut4<-as.numeric(minutes_cut4)*60+ as.numeric(seconds_cut4)

suturing_time4<-data$Suturing.Time.4
suturing_time_4<-as.character(suturing_time4[10])

minutes_sut4<-vapply(strsplit(suturing_time_4,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut4<-vapply(strsplit(suturing_time_4,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut4<-as.numeric(minutes_sut4)*60+ as.numeric(seconds_sut4)

#Session 5

cutting_time5<-data$Cutting.Time.5
cutting_time_5<-as.character(cutting_time5[10])

minutes_cut5<-vapply(strsplit(cutting_time_5,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut5<-vapply(strsplit(cutting_time_5,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut5<-as.numeric(minutes_cut5)*60+ as.numeric(seconds_cut5)

suturing_time5<-data$Suturing.Time.5
suturing_time_5<-as.character(suturing_time5[10])

minutes_sut5<-vapply(strsplit(suturing_time_5,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut5<-vapply(strsplit(suturing_time_5,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut5<-as.numeric(minutes_sut5)*60+ as.numeric(seconds_sut5)

Cutting_time = c(total_seconds_cut1,total_seconds_cut2,total_seconds_cut3,total_seconds_cut4,total_seconds_cut5)

Suturing_time = c(total_seconds_sut1,total_seconds_sut2,total_seconds_sut3,total_seconds_sut4,total_seconds_sut5)

data <- data.frame(CuttingTime=Cutting_time, SuturingTime=Suturing_time,row.names = c(1,2,3,4,5)) 

data

task.mat <- as.matrix(data)[,1:2]

barplot(t(task.mat),beside=TRUE,main="Subject13",col=c("gray","blue"),ylim=c(0,1300),ylab="Time (secs)")

#Subject11

#Session 1 

cutting_time1<-data$Cutting.Time.1
cutting_time_1<-as.character(cutting_time1[11])

minutes_cut1<-vapply(strsplit(cutting_time_1,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut1<-vapply(strsplit(cutting_time_1,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut1<-as.numeric(minutes_cut1)*60+ as.numeric(seconds_cut1)

suturing_time1<-data$Suturing.Time.1
suturing_time_1<-as.character(suturing_time1[11])

minutes_sut1<-vapply(strsplit(suturing_time_1,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut1<-vapply(strsplit(suturing_time_1,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut1<-as.numeric(minutes_sut1)*60+ as.numeric(seconds_sut1)

#Session 2

cutting_time2<-data$Cutting.Time.2
cutting_time_2<-as.character(cutting_time2[11])

minutes_cut2<-vapply(strsplit(cutting_time_2,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut2<-vapply(strsplit(cutting_time_2,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut2<-as.numeric(minutes_cut2)*60+ as.numeric(seconds_cut2)

suturing_time2<-data$Suturing.Time.2
suturing_time_2<-as.character(suturing_time2[11])

minutes_sut2<-vapply(strsplit(suturing_time_2,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut2<-vapply(strsplit(suturing_time_2,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut2<-as.numeric(minutes_sut2)*60+ as.numeric(seconds_sut2)

#Session 3

cutting_time3<-data$Cutting.Time.3
cutting_time_3<-as.character(cutting_time3[11])

minutes_cut3<-vapply(strsplit(cutting_time_3,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut3<-vapply(strsplit(cutting_time_3,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut3<-as.numeric(minutes_cut3)*60+ as.numeric(seconds_cut3)

suturing_time3<-data$Suturing.Time.3
suturing_time_3<-as.character(suturing_time3[11])

minutes_sut3<-vapply(strsplit(suturing_time_3,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut3<-vapply(strsplit(suturing_time_3,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut3<-as.numeric(minutes_sut3)*60+ as.numeric(seconds_sut3)

#Session 4

cutting_time4<-data$Cutting.Time.4
cutting_time_4<-as.character(cutting_time4[11])

minutes_cut4<-vapply(strsplit(cutting_time_4,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut4<-vapply(strsplit(cutting_time_4,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut4<-as.numeric(minutes_cut4)*60+ as.numeric(seconds_cut4)

suturing_time4<-data$Suturing.Time.4
suturing_time_4<-as.character(suturing_time4[11])

minutes_sut4<-vapply(strsplit(suturing_time_4,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut4<-vapply(strsplit(suturing_time_4,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut4<-as.numeric(minutes_sut4)*60+ as.numeric(seconds_sut4)

#Session 5

cutting_time5<-data$Cutting.Time.5
cutting_time_5<-as.character(cutting_time5[11])

minutes_cut5<-vapply(strsplit(cutting_time_5,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut5<-vapply(strsplit(cutting_time_5,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut5<-as.numeric(minutes_cut5)*60+ as.numeric(seconds_cut5)

suturing_time5<-data$Suturing.Time.5
suturing_time_5<-as.character(suturing_time5[11])

minutes_sut5<-vapply(strsplit(suturing_time_5,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut5<-vapply(strsplit(suturing_time_5,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut5<-as.numeric(minutes_sut5)*60+ as.numeric(seconds_sut5)

Cutting_time = c(total_seconds_cut1,total_seconds_cut2,total_seconds_cut3,total_seconds_cut4,total_seconds_cut5)

Suturing_time = c(total_seconds_sut1,total_seconds_sut2,total_seconds_sut3,total_seconds_sut4,total_seconds_sut5)

data <- data.frame(CuttingTime=Cutting_time, SuturingTime=Suturing_time,row.names = c(1,2,3,4,5)) 

data

task.mat <- as.matrix(data)[,1:2]

barplot(t(task.mat),beside=TRUE,main="Subject19",col=c("gray","blue"),ylim=c(0,1300))

#Subject12

#Session 1 

cutting_time1<-data$Cutting.Time.1
cutting_time_1<-as.character(cutting_time1[12])

minutes_cut1<-vapply(strsplit(cutting_time_1,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut1<-vapply(strsplit(cutting_time_1,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut1<-as.numeric(minutes_cut1)*60+ as.numeric(seconds_cut1)

suturing_time1<-data$Suturing.Time.1
suturing_time_1<-as.character(suturing_time1[12])

minutes_sut1<-vapply(strsplit(suturing_time_1,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut1<-vapply(strsplit(suturing_time_1,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut1<-as.numeric(minutes_sut1)*60+ as.numeric(seconds_sut1)

#Session 2

cutting_time2<-data$Cutting.Time.2
cutting_time_2<-as.character(cutting_time2[12])

minutes_cut2<-vapply(strsplit(cutting_time_2,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut2<-vapply(strsplit(cutting_time_2,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut2<-as.numeric(minutes_cut2)*60+ as.numeric(seconds_cut2)

suturing_time2<-data$Suturing.Time.2
suturing_time_2<-as.character(suturing_time2[12])

minutes_sut2<-vapply(strsplit(suturing_time_2,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut2<-vapply(strsplit(suturing_time_2,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut2<-as.numeric(minutes_sut2)*60+ as.numeric(seconds_sut2)

#Session 3

cutting_time3<-data$Cutting.Time.3
cutting_time_3<-as.character(cutting_time3[12])

minutes_cut3<-vapply(strsplit(cutting_time_3,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut3<-vapply(strsplit(cutting_time_3,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut3<-as.numeric(minutes_cut3)*60+ as.numeric(seconds_cut3)

suturing_time3<-data$Suturing.Time.3
suturing_time_3<-as.character(suturing_time3[12])

minutes_sut3<-vapply(strsplit(suturing_time_3,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut3<-vapply(strsplit(suturing_time_3,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut3<-as.numeric(minutes_sut3)*60+ as.numeric(seconds_sut3)

#Session 4

cutting_time4<-data$Cutting.Time.4
cutting_time_4<-as.character(cutting_time4[12])

minutes_cut4<-vapply(strsplit(cutting_time_4,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut4<-vapply(strsplit(cutting_time_4,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut4<-as.numeric(minutes_cut4)*60+ as.numeric(seconds_cut4)

suturing_time4<-data$Suturing.Time.4
suturing_time_4<-as.character(suturing_time4[12])

minutes_sut4<-vapply(strsplit(suturing_time_4,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut4<-vapply(strsplit(suturing_time_4,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut4<-as.numeric(minutes_sut4)*60+ as.numeric(seconds_sut4)

#Session 5

cutting_time5<-data$Cutting.Time.5
cutting_time_5<-as.character(cutting_time5[12])

minutes_cut5<-vapply(strsplit(cutting_time_5,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut5<-vapply(strsplit(cutting_time_5,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut5<-as.numeric(minutes_cut5)*60+ as.numeric(seconds_cut5)

suturing_time5<-data$Suturing.Time.5
suturing_time_5<-as.character(suturing_time5[12])

minutes_sut5<-vapply(strsplit(suturing_time_5,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut5<-vapply(strsplit(suturing_time_5,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut5<-as.numeric(minutes_sut5)*60+ as.numeric(seconds_sut5)

Cutting_time = c(total_seconds_cut1,total_seconds_cut2,total_seconds_cut3,total_seconds_cut4,total_seconds_cut5)

Suturing_time = c(total_seconds_sut1,total_seconds_sut2,total_seconds_sut3,total_seconds_sut4,total_seconds_sut5)

data <- data.frame(CuttingTime=Cutting_time, SuturingTime=Suturing_time,row.names = c(1,2,3,4,5)) 

data

task.mat <- as.matrix(data)[,1:2]

barplot(t(task.mat),beside=TRUE,main="Subject21",col=c("gray","blue"),ylim=c(0,1300))

#Subject13

#Session 1 

cutting_time1<-data$Cutting.Time.1
cutting_time_1<-as.character(cutting_time1[13])

minutes_cut1<-vapply(strsplit(cutting_time_1,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut1<-vapply(strsplit(cutting_time_1,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut1<-as.numeric(minutes_cut1)*60+ as.numeric(seconds_cut1)

suturing_time1<-data$Suturing.Time.1
suturing_time_1<-as.character(suturing_time1[13])

minutes_sut1<-vapply(strsplit(suturing_time_1,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut1<-vapply(strsplit(suturing_time_1,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut1<-as.numeric(minutes_sut1)*60+ as.numeric(seconds_sut1)

#Session 2

cutting_time2<-data$Cutting.Time.2
cutting_time_2<-as.character(cutting_time2[13])

minutes_cut2<-vapply(strsplit(cutting_time_2,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut2<-vapply(strsplit(cutting_time_2,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut2<-as.numeric(minutes_cut2)*60+ as.numeric(seconds_cut2)

suturing_time2<-data$Suturing.Time.2
suturing_time_2<-as.character(suturing_time2[13])

minutes_sut2<-vapply(strsplit(suturing_time_2,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut2<-vapply(strsplit(suturing_time_2,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut2<-as.numeric(minutes_sut2)*60+ as.numeric(seconds_sut2)

#Session 3

cutting_time3<-data$Cutting.Time.3
cutting_time_3<-as.character(cutting_time3[13])

minutes_cut3<-vapply(strsplit(cutting_time_3,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut3<-vapply(strsplit(cutting_time_3,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut3<-as.numeric(minutes_cut3)*60+ as.numeric(seconds_cut3)

suturing_time3<-data$Suturing.Time.3
suturing_time_3<-as.character(suturing_time3[13])

minutes_sut3<-vapply(strsplit(suturing_time_3,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut3<-vapply(strsplit(suturing_time_3,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut3<-as.numeric(minutes_sut3)*60+ as.numeric(seconds_sut3)

#Session 4

cutting_time4<-data$Cutting.Time.4
cutting_time_4<-as.character(cutting_time4[13])

minutes_cut4<-vapply(strsplit(cutting_time_4,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut4<-vapply(strsplit(cutting_time_4,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut4<-as.numeric(minutes_cut4)*60+ as.numeric(seconds_cut4)

suturing_time4<-data$Suturing.Time.4
suturing_time_4<-as.character(suturing_time4[13])

minutes_sut4<-vapply(strsplit(suturing_time_4,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut4<-vapply(strsplit(suturing_time_4,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut4<-as.numeric(minutes_sut4)*60+ as.numeric(seconds_sut4)

#Session 5

cutting_time5<-data$Cutting.Time.5
cutting_time_5<-as.character(cutting_time5[13])

minutes_cut5<-vapply(strsplit(cutting_time_5,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut5<-vapply(strsplit(cutting_time_5,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut5<-as.numeric(minutes_cut5)*60+ as.numeric(seconds_cut5)

suturing_time5<-data$Suturing.Time.5
suturing_time_5<-as.character(suturing_time5[13])

minutes_sut5<-vapply(strsplit(suturing_time_5,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut5<-vapply(strsplit(suturing_time_5,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut5<-as.numeric(minutes_sut5)*60+ as.numeric(seconds_sut5)

Cutting_time = c(total_seconds_cut1,total_seconds_cut2,total_seconds_cut3,total_seconds_cut4,total_seconds_cut5)

Suturing_time = c(total_seconds_sut1,total_seconds_sut2,total_seconds_sut3,total_seconds_sut4,total_seconds_sut5)

data <- data.frame(CuttingTime=Cutting_time, SuturingTime=Suturing_time,row.names = c(1,2,3,4,5)) 

data

task.mat <- as.matrix(data)[,1:2]

barplot(t(task.mat),beside=TRUE,main="Subject22",col=c("gray","blue"),ylim=c(0,1300),xlab="Session",ylab="Time(secs)")

#Subject14

#Session 1 

cutting_time1<-data$Cutting.Time.1
cutting_time_1<-as.character(cutting_time1[14])

minutes_cut1<-vapply(strsplit(cutting_time_1,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut1<-vapply(strsplit(cutting_time_1,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut1<-as.numeric(minutes_cut1)*60+ as.numeric(seconds_cut1)

suturing_time1<-data$Suturing.Time.1
suturing_time_1<-as.character(suturing_time1[14])

minutes_sut1<-vapply(strsplit(suturing_time_1,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut1<-vapply(strsplit(suturing_time_1,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut1<-as.numeric(minutes_sut1)*60+ as.numeric(seconds_sut1)

#Session 2

cutting_time2<-data$Cutting.Time.2
cutting_time_2<-as.character(cutting_time2[14])

minutes_cut2<-vapply(strsplit(cutting_time_2,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut2<-vapply(strsplit(cutting_time_2,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut2<-as.numeric(minutes_cut2)*60+ as.numeric(seconds_cut2)

suturing_time2<-data$Suturing.Time.2
suturing_time_2<-as.character(suturing_time2[14])

minutes_sut2<-vapply(strsplit(suturing_time_2,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut2<-vapply(strsplit(suturing_time_2,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut2<-as.numeric(minutes_sut2)*60+ as.numeric(seconds_sut2)

#Session 3

cutting_time3<-data$Cutting.Time.3
cutting_time_3<-as.character(cutting_time3[14])

minutes_cut3<-vapply(strsplit(cutting_time_3,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut3<-vapply(strsplit(cutting_time_3,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut3<-as.numeric(minutes_cut3)*60+ as.numeric(seconds_cut3)

suturing_time3<-data$Suturing.Time.3
suturing_time_3<-as.character(suturing_time3[14])

minutes_sut3<-vapply(strsplit(suturing_time_3,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut3<-vapply(strsplit(suturing_time_3,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut3<-as.numeric(minutes_sut3)*60+ as.numeric(seconds_sut3)

#Session 4

cutting_time4<-data$Cutting.Time.4
cutting_time_4<-as.character(cutting_time4[14])

minutes_cut4<-vapply(strsplit(cutting_time_4,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut4<-vapply(strsplit(cutting_time_4,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut4<-as.numeric(minutes_cut4)*60+ as.numeric(seconds_cut4)

suturing_time4<-data$Suturing.Time.4
suturing_time_4<-as.character(suturing_time4[14])

minutes_sut4<-vapply(strsplit(suturing_time_4,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut4<-vapply(strsplit(suturing_time_4,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut4<-as.numeric(minutes_sut4)*60+ as.numeric(seconds_sut4)

#Session 5

cutting_time5<-data$Cutting.Time.5
cutting_time_5<-as.character(cutting_time5[14])

minutes_cut5<-vapply(strsplit(cutting_time_5,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut5<-vapply(strsplit(cutting_time_5,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut5<-as.numeric(minutes_cut5)*60+ as.numeric(seconds_cut5)

suturing_time5<-data$Suturing.Time.5
suturing_time_5<-as.character(suturing_time5[14])

minutes_sut5<-vapply(strsplit(suturing_time_5,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut5<-vapply(strsplit(suturing_time_5,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut5<-as.numeric(minutes_sut5)*60+ as.numeric(seconds_sut5)

Cutting_time = c(total_seconds_cut1,total_seconds_cut2,total_seconds_cut3,total_seconds_cut4,total_seconds_cut5)

Suturing_time = c(total_seconds_sut1,total_seconds_sut2,total_seconds_sut3,total_seconds_sut4,total_seconds_sut5)

data <- data.frame(CuttingTime=Cutting_time, SuturingTime=Suturing_time,row.names = c(1,2,3,4,5)) 

data

task.mat <- as.matrix(data)[,1:2]

barplot(t(task.mat),beside=TRUE,main="Subject24",col=c("gray","blue"),ylim=c(0,1300),xlab="Session")

#Subject15

#Session 1 

cutting_time1<-data$Cutting.Time.1
cutting_time_1<-as.character(cutting_time1[15])

minutes_cut1<-vapply(strsplit(cutting_time_1,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut1<-vapply(strsplit(cutting_time_1,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut1<-as.numeric(minutes_cut1)*60+ as.numeric(seconds_cut1)

suturing_time1<-data$Suturing.Time.1
suturing_time_1<-as.character(suturing_time1[15])

minutes_sut1<-vapply(strsplit(suturing_time_1,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut1<-vapply(strsplit(suturing_time_1,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut1<-as.numeric(minutes_sut1)*60+ as.numeric(seconds_sut1)

#Session 2

cutting_time2<-data$Cutting.Time.2
cutting_time_2<-as.character(cutting_time2[15])

minutes_cut2<-vapply(strsplit(cutting_time_2,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut2<-vapply(strsplit(cutting_time_2,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut2<-as.numeric(minutes_cut2)*60+ as.numeric(seconds_cut2)

suturing_time2<-data$Suturing.Time.2
suturing_time_2<-as.character(suturing_time2[15])

minutes_sut2<-vapply(strsplit(suturing_time_2,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut2<-vapply(strsplit(suturing_time_2,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut2<-as.numeric(minutes_sut2)*60+ as.numeric(seconds_sut2)

#Session 3

cutting_time3<-data$Cutting.Time.3
cutting_time_3<-as.character(cutting_time3[15])

minutes_cut3<-vapply(strsplit(cutting_time_3,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut3<-vapply(strsplit(cutting_time_3,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut3<-as.numeric(minutes_cut3)*60+ as.numeric(seconds_cut3)

suturing_time3<-data$Suturing.Time.3
suturing_time_3<-as.character(suturing_time3[15])

minutes_sut3<-vapply(strsplit(suturing_time_3,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut3<-vapply(strsplit(suturing_time_3,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut3<-as.numeric(minutes_sut3)*60+ as.numeric(seconds_sut3)

#Session 4

cutting_time4<-data$Cutting.Time.4
cutting_time_4<-as.character(cutting_time4[15])

minutes_cut4<-vapply(strsplit(cutting_time_4,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut4<-vapply(strsplit(cutting_time_4,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut4<-as.numeric(minutes_cut4)*60+ as.numeric(seconds_cut4)

suturing_time4<-data$Suturing.Time.4
suturing_time_4<-as.character(suturing_time4[15])

minutes_sut4<-vapply(strsplit(suturing_time_4,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut4<-vapply(strsplit(suturing_time_4,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut4<-as.numeric(minutes_sut4)*60+ as.numeric(seconds_sut4)

#Session 5

cutting_time5<-data$Cutting.Time.5
cutting_time_5<-as.character(cutting_time5[15])

minutes_cut5<-vapply(strsplit(cutting_time_5,":"), `[`, 1, FUN.VALUE=character(1))
seconds_cut5<-vapply(strsplit(cutting_time_5,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_cut5<-as.numeric(minutes_cut5)*60+ as.numeric(seconds_cut5)

suturing_time5<-data$Suturing.Time.5
suturing_time_5<-as.character(suturing_time5[15])

minutes_sut5<-vapply(strsplit(suturing_time_5,":"), `[`, 1, FUN.VALUE=character(1))
seconds_sut5<-vapply(strsplit(suturing_time_5,":"), `[`, 2, FUN.VALUE=character(1))
total_seconds_sut5<-as.numeric(minutes_sut5)*60+ as.numeric(seconds_sut5)

Cutting_time = c(total_seconds_cut1,total_seconds_cut2,total_seconds_cut3,total_seconds_cut4,total_seconds_cut5)

Suturing_time = c(total_seconds_sut1,total_seconds_sut2,total_seconds_sut3,total_seconds_sut4,total_seconds_sut5)

data <- data.frame(CuttingTime=Cutting_time, SuturingTime=Suturing_time,row.names = c(1,2,3,4,5)) 

data

task.mat <- as.matrix(data)[,1:2]

barplot(t(task.mat),beside=TRUE,main="Subject26",col=c("gray","blue"),ylim=c(0,1300),xlab="Session")

title(main="Time Bar Plot-Cutting Vs Suturing",outer=T,xlab="Session",ylab="Time(secs)")