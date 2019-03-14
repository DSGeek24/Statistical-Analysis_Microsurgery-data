performance_data<-read.csv(file="D:/Downloads/osfstorage-archive/Methodist microsurgery with output/MicrosurgeryPerformance.csv")

#..........Scorer 2.................................

library(ggplot2)
par(mfrow=c(3,5))
par(oma=c(0,0,2,0))

#Subject1

#Session 1 

cut1_score<-performance_data$Score.Cut2
sut1_score<-performance_data$Score.Sut2

cutting1_score<-as.numeric(as.character(cut1_score[1]))
suturing1_score<-as.numeric(as.character(sut1_score[1]))

#Session 2

cut2_score<-performance_data$Score.Cut2.1
sut2_score<-performance_data$Score.Sut2.1

cutting2_score<-as.numeric(as.character(cut2_score[1]))
suturing2_score<-as.numeric(as.character(sut2_score[1]))

#Session 3

cut3_score<-performance_data$Score.Cut2.2
sut3_score<-performance_data$Score.Sut2.2

cutting3_score<-as.numeric(as.character(cut3_score[1]))
suturing3_score<-as.numeric(as.character(sut3_score[1]))

#Session 4

cut4_score<-performance_data$Score.Cut2.3
sut4_score<-performance_data$Score.Sut2.3

cutting4_score<-as.numeric(as.character(cut4_score[1]))
suturing4_score<-as.numeric(as.character(sut4_score[1]))

#Session 5

cut5_score<-performance_data$Score.Cut2.4
sut5_score<-performance_data$Score.Sut2.4

cutting5_score<-as.numeric(as.character(cut5_score[1]))
suturing5_score<-as.numeric(as.character(sut5_score[1]))

Cutting_Scores1 = c(cutting1_score,cutting2_score,cutting3_score,cutting4_score,cutting5_score)
Suturing_Scores1=c(suturing1_score,suturing2_score,suturing3_score,suturing4_score,suturing5_score)

task_scores <- data.frame(CuttingScore=Cutting_Scores1, SuturingScore=Suturing_Scores1,row.names = c(1,2,3,4,5)) 

task.mat <- as.matrix(task_scores)[,1:2]

barplot(t(task.mat),beside=TRUE,main="Subject1",col=c("green","blue"),ylim=c(0,30),ylab="Score")


#Subject2

#Session 1 

cut1_score<-performance_data$Score.Cut2
sut1_score<-performance_data$Score.Sut2

cutting1_score<-as.numeric(as.character(cut1_score[2]))
suturing1_score<-as.numeric(as.character(sut1_score[2]))

#Session 2

cut2_score<-performance_data$Score.Cut2.1
sut2_score<-performance_data$Score.Sut2.1

cutting2_score<-as.numeric(as.character(cut2_score[2]))
suturing2_score<-as.numeric(as.character(sut2_score[2]))

#Session 3

cut3_score<-performance_data$Score.Cut2.2
sut3_score<-performance_data$Score.Sut2.2

cutting3_score<-as.numeric(as.character(cut3_score[2]))
suturing3_score<-as.numeric(as.character(sut3_score[2]))

#Session 4

cut4_score<-performance_data$Score.Cut2.3
sut4_score<-performance_data$Score.Sut2.3

cutting4_score<-as.numeric(as.character(cut4_score[2]))
suturing4_score<-as.numeric(as.character(sut4_score[2]))

#Session 5

cut5_score<-performance_data$Score.Cut2.4
sut5_score<-performance_data$Score.Sut2.4

cutting5_score<-as.numeric(as.character(cut5_score[2]))
suturing5_score<-as.numeric(as.character(sut5_score[2]))

Cutting_Scores1 = c(cutting1_score,cutting2_score,cutting3_score,cutting4_score,cutting5_score)
Suturing_Scores1=c(suturing1_score,suturing2_score,suturing3_score,suturing4_score,suturing5_score)

task_scores <- data.frame(CuttingScore=Cutting_Scores1, SuturingScore=Suturing_Scores1,row.names = c(1,2,3,4,5)) 

task.mat <- as.matrix(task_scores)[,1:2]

barplot(t(task.mat),beside=TRUE,main="Subject2",col=c("green","blue"),ylim=c(0,30),axes=F)

#Subject3

#Session 1 

cut1_score<-performance_data$Score.Cut2
sut1_score<-performance_data$Score.Sut2

cutting1_score<-as.numeric(as.character(cut1_score[3]))
suturing1_score<-as.numeric(as.character(sut1_score[3]))

#Session 2

cut2_score<-performance_data$Score.Cut2.1
sut2_score<-performance_data$Score.Sut2.1

cutting2_score<-as.numeric(as.character(cut2_score[3]))
suturing2_score<-as.numeric(as.character(sut2_score[3]))

#Session 3

cut3_score<-performance_data$Score.Cut2.2
sut3_score<-performance_data$Score.Sut2.2

cutting3_score<-as.numeric(as.character(cut3_score[3]))
suturing3_score<-as.numeric(as.character(sut3_score[3]))

#Session 4

cut4_score<-performance_data$Score.Cut2.3
sut4_score<-performance_data$Score.Sut2.3

cutting4_score<-as.numeric(as.character(cut4_score[3]))
suturing4_score<-as.numeric(as.character(sut4_score[3]))

#Session 5

cut5_score<-performance_data$Score.Cut2.4
sut5_score<-performance_data$Score.Sut2.4

cutting5_score<-as.numeric(as.character(cut5_score[3]))
suturing5_score<-as.numeric(as.character(sut5_score[3]))

Cutting_Scores1 = c(cutting1_score,cutting2_score,cutting3_score,cutting4_score,cutting5_score)
Suturing_Scores1=c(suturing1_score,suturing2_score,suturing3_score,suturing4_score,suturing5_score)

task_scores <- data.frame(CuttingScore=Cutting_Scores1, SuturingScore=Suturing_Scores1,row.names = c(1,2,3,4,5)) 

task.mat <- as.matrix(task_scores)[,1:2]
barplot(t(task.mat),beside=TRUE,main="Subject3",col=c("green","blue"),ylim=c(0,30),axes=F)

#Subject4

#Session 1 

cut1_score<-performance_data$Score.Cut2
sut1_score<-performance_data$Score.Sut2

cutting1_score<-as.numeric(as.character(cut1_score[4]))
suturing1_score<-as.numeric(as.character(sut1_score[4]))

#Session 2

cut2_score<-performance_data$Score.Cut2.1
sut2_score<-performance_data$Score.Sut2.1

cutting2_score<-as.numeric(as.character(cut2_score[4]))
suturing2_score<-as.numeric(as.character(sut2_score[4]))

#Session 3

cut3_score<-performance_data$Score.Cut2.2
sut3_score<-performance_data$Score.Sut2.2

cutting3_score<-as.numeric(as.character(cut3_score[4]))
suturing3_score<-as.numeric(as.character(sut3_score[4]))

#Session 4

cut4_score<-performance_data$Score.Cut2.3
sut4_score<-performance_data$Score.Sut2.3

cutting4_score<-as.numeric(as.character(cut4_score[4]))
suturing4_score<-as.numeric(as.character(sut4_score[4]))

#Session 5

cut5_score<-performance_data$Score.Cut2.4
sut5_score<-performance_data$Score.Sut2.4

cutting5_score<-as.numeric(as.character(cut5_score[4]))
suturing5_score<-as.numeric(as.character(sut5_score[4]))

Cutting_Scores1 = c(cutting1_score,cutting2_score,cutting3_score,cutting4_score,cutting5_score)
Suturing_Scores1=c(suturing1_score,suturing2_score,suturing3_score,suturing4_score,suturing5_score)

task_scores <- data.frame(CuttingScore=Cutting_Scores1, SuturingScore=Suturing_Scores1,row.names = c(1,2,3,4,5)) 

task.mat <- as.matrix(task_scores)[,1:2]
barplot(t(task.mat),beside=TRUE,main="Subject4",col=c("green","blue"),ylab="Score",ylim=c(0,30))

#Subject5

#Session 1 

cut1_score<-performance_data$Score.Cut2
sut1_score<-performance_data$Score.Sut2

cutting1_score<-as.numeric(as.character(cut1_score[5]))
suturing1_score<-as.numeric(as.character(sut1_score[5]))

#Session 2

cut2_score<-performance_data$Score.Cut2.1
sut2_score<-performance_data$Score.Sut2.1

cutting2_score<-as.numeric(as.character(cut2_score[5]))
suturing2_score<-as.numeric(as.character(sut2_score[5]))

#Session 3

cut3_score<-performance_data$Score.Cut2.2
sut3_score<-performance_data$Score.Sut2.2

cutting3_score<-as.numeric(as.character(cut3_score[5]))
suturing3_score<-as.numeric(as.character(sut3_score[5]))

#Session 4

cut4_score<-performance_data$Score.Cut2.3
sut4_score<-performance_data$Score.Sut2.3

cutting4_score<-as.numeric(as.character(cut4_score[5]))
suturing4_score<-as.numeric(as.character(sut4_score[5]))

#Session 5

cut5_score<-performance_data$Score.Cut2.4
sut5_score<-performance_data$Score.Sut2.4

cutting5_score<-as.numeric(as.character(cut5_score[5]))
suturing5_score<-as.numeric(as.character(sut5_score[5]))

Cutting_Scores1 = c(cutting1_score,cutting2_score,cutting3_score,cutting4_score,cutting5_score)
Suturing_Scores1=c(suturing1_score,suturing2_score,suturing3_score,suturing4_score,suturing5_score)

task_scores <- data.frame(CuttingScore=Cutting_Scores1, SuturingScore=Suturing_Scores1,row.names = c(1,2,3,4,5)) 

task.mat <- as.matrix(task_scores)[,1:2]

barplot(t(task.mat),beside=TRUE,main="Subject7",col=c("green","blue"),ylim=c(0,30))

#Subject6

#Session 1 

cut1_score<-performance_data$Score.Cut2
sut1_score<-performance_data$Score.Sut2

cutting1_score<-as.numeric(as.character(cut1_score[6]))
suturing1_score<-as.numeric(as.character(sut1_score[6]))

#Session 2

cut2_score<-performance_data$Score.Cut2.1
sut2_score<-performance_data$Score.Sut2.1

cutting2_score<-as.numeric(as.character(cut2_score[6]))
suturing2_score<-as.numeric(as.character(sut2_score[6]))

#Session 3

cut3_score<-performance_data$Score.Cut2.2
sut3_score<-performance_data$Score.Sut2.2

cutting3_score<-as.numeric(as.character(cut3_score[6]))
suturing3_score<-as.numeric(as.character(sut3_score[6]))

#Session 4

cut4_score<-performance_data$Score.Cut2.3
sut4_score<-performance_data$Score.Sut2.3

cutting4_score<-as.numeric(as.character(cut4_score[6]))
suturing4_score<-as.numeric(as.character(sut4_score[6]))

#Session 5

cut5_score<-performance_data$Score.Cut2.4
sut5_score<-performance_data$Score.Sut2.4

cutting5_score<-as.numeric(as.character(cut5_score[6]))
suturing5_score<-as.numeric(as.character(sut5_score[6]))

Cutting_Scores1 = c(cutting1_score,cutting2_score,cutting3_score,cutting4_score,cutting5_score)
Suturing_Scores1=c(suturing1_score,suturing2_score,suturing3_score,suturing4_score,suturing5_score)

task_scores <- data.frame(CuttingScore=Cutting_Scores1, SuturingScore=Suturing_Scores1,row.names = c(1,2,3,4,5)) 

task.mat <- as.matrix(task_scores)[,1:2]

barplot(t(task.mat),beside=TRUE,main="Subject8",col=c("green","blue"),ylim=c(0,30))

#Subject7

#Session 1 

cut1_score<-performance_data$Score.Cut2
sut1_score<-performance_data$Score.Sut2

cutting1_score<-as.numeric(as.character(cut1_score[7]))
suturing1_score<-as.numeric(as.character(sut1_score[7]))

#Session 2

cut2_score<-performance_data$Score.Cut2.1
sut2_score<-performance_data$Score.Sut2.1

cutting2_score<-as.numeric(as.character(cut2_score[7]))
suturing2_score<-as.numeric(as.character(sut2_score[7]))

#Session 3

cut3_score<-performance_data$Score.Cut2.2
sut3_score<-performance_data$Score.Sut2.2

cutting3_score<-as.numeric(as.character(cut3_score[7]))
suturing3_score<-as.numeric(as.character(sut3_score[7]))

#Session 4

cut4_score<-performance_data$Score.Cut2.3
sut4_score<-performance_data$Score.Sut2.3

cutting4_score<-as.numeric(as.character(cut4_score[7]))
suturing4_score<-as.numeric(as.character(sut4_score[7]))

#Session 5

cut5_score<-performance_data$Score.Cut2.4
sut5_score<-performance_data$Score.Sut2.4

cutting5_score<-as.numeric(as.character(cut5_score[7]))
suturing5_score<-as.numeric(as.character(sut5_score[7]))

Cutting_Scores1 = c(cutting1_score,cutting2_score,cutting3_score,cutting4_score,cutting5_score)
Suturing_Scores1=c(suturing1_score,suturing2_score,suturing3_score,suturing4_score,suturing5_score)

task_scores <- data.frame(CuttingScore=Cutting_Scores1, SuturingScore=Suturing_Scores1,row.names = c(1,2,3,4,5)) 

task.mat <- as.matrix(task_scores)[,1:2]

barplot(t(task.mat),beside=TRUE,main="Subject10",col=c("green","blue"),ylab="Score",xlab="Session",ylim=c(0,30))

#Subject8

#Session 1 

cut1_score<-performance_data$Score.Cut2
sut1_score<-performance_data$Score.Sut2

cutting1_score<-as.numeric(as.character(cut1_score[8]))
suturing1_score<-as.numeric(as.character(sut1_score[8]))

#Session 2

cut2_score<-performance_data$Score.Cut2.1
sut2_score<-performance_data$Score.Sut2.1

cutting2_score<-as.numeric(as.character(cut2_score[8]))
suturing2_score<-as.numeric(as.character(sut2_score[8]))

#Session 3

cut3_score<-performance_data$Score.Cut2.2
sut3_score<-performance_data$Score.Sut2.2

cutting3_score<-as.numeric(as.character(cut3_score[8]))
suturing3_score<-as.numeric(as.character(sut3_score[8]))

#Session 4

cut4_score<-performance_data$Score.Cut2.3
sut4_score<-performance_data$Score.Sut2.3

cutting4_score<-as.numeric(as.character(cut4_score[8]))
suturing4_score<-as.numeric(as.character(sut4_score[8]))

#Session 5

cut5_score<-performance_data$Score.Cut2.4
sut5_score<-performance_data$Score.Sut2.4

cutting5_score<-as.numeric(as.character(cut5_score[8]))
suturing5_score<-as.numeric(as.character(sut5_score[8]))

Cutting_Scores1 = c(cutting1_score,cutting2_score,cutting3_score,cutting4_score,cutting5_score)
Suturing_Scores1=c(suturing1_score,suturing2_score,suturing3_score,suturing4_score,suturing5_score)

task_scores <- data.frame(CuttingScore=Cutting_Scores1, SuturingScore=Suturing_Scores1,row.names = c(1,2,3,4,5)) 

task.mat <- as.matrix(task_scores)[,1:2]

barplot(t(task.mat),beside=TRUE,main="Subject11",col=c("green","blue"),ylim=c(0,30),xlab="Session")

#Subject9

#Session 1 

cut1_score<-performance_data$Score.Cut2
sut1_score<-performance_data$Score.Sut2

cutting1_score<-as.numeric(as.character(cut1_score[9]))
suturing1_score<-as.numeric(as.character(sut1_score[9]))

#Session 2

cut2_score<-performance_data$Score.Cut2.1
sut2_score<-performance_data$Score.Sut2.1

cutting2_score<-as.numeric(as.character(cut2_score[9]))
suturing2_score<-as.numeric(as.character(sut2_score[9]))

#Session 3

cut3_score<-performance_data$Score.Cut2.2
sut3_score<-performance_data$Score.Sut2.2

cutting3_score<-as.numeric(as.character(cut3_score[9]))
suturing3_score<-as.numeric(as.character(sut3_score[9]))

#Session 4

cut4_score<-performance_data$Score.Cut2.3
sut4_score<-performance_data$Score.Sut2.3

cutting4_score<-as.numeric(as.character(cut4_score[9]))
suturing4_score<-as.numeric(as.character(sut4_score[9]))

#Session 5

cut5_score<-performance_data$Score.Cut2.4
sut5_score<-performance_data$Score.Sut2.4

cutting5_score<-as.numeric(as.character(cut5_score[9]))
suturing5_score<-as.numeric(as.character(sut5_score[9]))

Cutting_Scores1 = c(cutting1_score,cutting2_score,cutting3_score,cutting4_score,cutting5_score)
Suturing_Scores1=c(suturing1_score,suturing2_score,suturing3_score,suturing4_score,suturing5_score)

task_scores <- data.frame(CuttingScore=Cutting_Scores1, SuturingScore=Suturing_Scores1,row.names = c(1,2,3,4,5)) 

task.mat <- as.matrix(task_scores)[,1:2]

barplot(t(task.mat),beside=TRUE,main="Subject12",col=c("green","blue"),ylim=c(0,30),xlab="Session")

title(main="Accuracy Bar Plot-Cutting Vs Suturing as per Scorer2",outer=T,xlab="Session",ylab="Score")
###########################################################################################################33

####Batch 2#################################################

library(ggplot2)
par(mfrow=c(2,3))
par(oma=c(0,0,2,0))


#Subject10

#Session 1 

cut1_score<-performance_data$Score.Cut2
sut1_score<-performance_data$Score.Sut2

cutting1_score<-as.numeric(as.character(cut1_score[10]))
suturing1_score<-as.numeric(as.character(sut1_score[10]))

#Session 2

cut2_score<-performance_data$Score.Cut2.1
sut2_score<-performance_data$Score.Sut2.1

cutting2_score<-as.numeric(as.character(cut2_score[10]))
suturing2_score<-as.numeric(as.character(sut2_score[10]))

#Session 3

cut3_score<-performance_data$Score.Cut2.2
sut3_score<-performance_data$Score.Sut2.2

cutting3_score<-as.numeric(as.character(cut3_score[10]))
suturing3_score<-as.numeric(as.character(sut3_score[10]))

#Session 4

cut4_score<-performance_data$Score.Cut2.3
sut4_score<-performance_data$Score.Sut2.3

cutting4_score<-as.numeric(as.character(cut4_score[10]))
suturing4_score<-as.numeric(as.character(sut4_score[10]))

#Session 5

cut5_score<-performance_data$Score.Cut2.4
sut5_score<-performance_data$Score.Sut2.4

cutting5_score<-as.numeric(as.character(cut5_score[10]))
suturing5_score<-as.numeric(as.character(sut5_score[10]))

Cutting_Scores1 = c(cutting1_score,cutting2_score,cutting3_score,cutting4_score,cutting5_score)
Suturing_Scores1=c(suturing1_score,suturing2_score,suturing3_score,suturing4_score,suturing5_score)

task_scores <- data.frame(CuttingScore=Cutting_Scores1, SuturingScore=Suturing_Scores1,row.names = c(1,2,3,4,5)) 

task.mat <- as.matrix(task_scores)[,1:2]

barplot(t(task.mat),beside=TRUE,main="Subject13",col=c("green","blue"),ylim=c(0,30),ylab="Score")

#Subject11

#Session 1 

cut1_score<-performance_data$Score.Cut2
sut1_score<-performance_data$Score.Sut2

cutting1_score<-as.numeric(as.character(cut1_score[11]))
suturing1_score<-as.numeric(as.character(sut1_score[11]))

#Session 2

cut2_score<-performance_data$Score.Cut2.1
sut2_score<-performance_data$Score.Sut2.1

cutting2_score<-as.numeric(as.character(cut2_score[11]))
suturing2_score<-as.numeric(as.character(sut2_score[11]))

#Session 3

cut3_score<-performance_data$Score.Cut2.2
sut3_score<-performance_data$Score.Sut2.2

cutting3_score<-as.numeric(as.character(cut3_score[11]))
suturing3_score<-as.numeric(as.character(sut3_score[11]))

#Session 4

cut4_score<-performance_data$Score.Cut2.3
sut4_score<-performance_data$Score.Sut2.3

cutting4_score<-as.numeric(as.character(cut4_score[11]))
suturing4_score<-as.numeric(as.character(sut4_score[11]))

#Session 5

cut5_score<-performance_data$Score.Cut2.4
sut5_score<-performance_data$Score.Sut2.4

cutting5_score<-as.numeric(as.character(cut5_score[11]))
suturing5_score<-as.numeric(as.character(sut5_score[11]))

Cutting_Scores1 = c(cutting1_score,cutting2_score,cutting3_score,cutting4_score,cutting5_score)
Suturing_Scores1=c(suturing1_score,suturing2_score,suturing3_score,suturing4_score,suturing5_score)

task_scores <- data.frame(CuttingScore=Cutting_Scores1, SuturingScore=Suturing_Scores1,row.names = c(1,2,3,4,5)) 

task.mat <- as.matrix(task_scores)[,1:2]

barplot(t(task.mat),beside=TRUE,main="Subject19",col=c("green","blue"),ylim=c(0,30))

#Subject12

#Session 1 

cut1_score<-performance_data$Score.Cut2
sut1_score<-performance_data$Score.Sut2

cutting1_score<-as.numeric(as.character(cut1_score[12]))
suturing1_score<-as.numeric(as.character(sut1_score[12]))

#Session 2

cut2_score<-performance_data$Score.Cut2.1
sut2_score<-performance_data$Score.Sut2.1

cutting2_score<-as.numeric(as.character(cut2_score[12]))
suturing2_score<-as.numeric(as.character(sut2_score[12]))

#Session 3

cut3_score<-performance_data$Score.Cut2.2
sut3_score<-performance_data$Score.Sut2.2

cutting3_score<-as.numeric(as.character(cut3_score[12]))
suturing3_score<-as.numeric(as.character(sut3_score[12]))

#Session 4

cut4_score<-performance_data$Score.Cut2.3
sut4_score<-performance_data$Score.Sut2.3

cutting4_score<-as.numeric(as.character(cut4_score[12]))
suturing4_score<-as.numeric(as.character(sut4_score[12]))

#Session 5

cut5_score<-performance_data$Score.Cut2.4
sut5_score<-performance_data$Score.Sut2.4

cutting5_score<-as.numeric(as.character(cut5_score[12]))
suturing5_score<-as.numeric(as.character(sut5_score[12]))

Cutting_Scores1 = c(cutting1_score,cutting2_score,cutting3_score,cutting4_score,cutting5_score)
Suturing_Scores1=c(suturing1_score,suturing2_score,suturing3_score,suturing4_score,suturing5_score)

task_scores <- data.frame(CuttingScore=Cutting_Scores1, SuturingScore=Suturing_Scores1,row.names = c(1,2,3,4,5)) 

task.mat <- as.matrix(task_scores)[,1:2]

barplot(t(task.mat),beside=TRUE,main="Subject21",col=c("green","blue"),ylim=c(0,30))

#Subject13

#Session 1 

cut1_score<-performance_data$Score.Cut2
sut1_score<-performance_data$Score.Sut2

cutting1_score<-as.numeric(as.character(cut1_score[13]))
suturing1_score<-as.numeric(as.character(sut1_score[13]))

#Session 2

cut2_score<-performance_data$Score.Cut2.1
sut2_score<-performance_data$Score.Sut2.1

cutting2_score<-as.numeric(as.character(cut2_score[13]))
suturing2_score<-as.numeric(as.character(sut2_score[13]))

#Session 3

cut3_score<-performance_data$Score.Cut2.2
sut3_score<-performance_data$Score.Sut2.2

cutting3_score<-as.numeric(as.character(cut3_score[13]))
suturing3_score<-as.numeric(as.character(sut3_score[13]))

#Session 4

cut4_score<-performance_data$Score.Cut2.3
sut4_score<-performance_data$Score.Sut2.3

cutting4_score<-as.numeric(as.character(cut4_score[13]))
suturing4_score<-as.numeric(as.character(sut4_score[13]))

#Session 5

cut5_score<-performance_data$Score.Cut2.4
sut5_score<-performance_data$Score.Sut2.4

cutting5_score<-as.numeric(as.character(cut5_score[13]))
suturing5_score<-as.numeric(as.character(sut5_score[13]))

Cutting_Scores1 = c(cutting1_score,cutting2_score,cutting3_score,cutting4_score,cutting5_score)
Suturing_Scores1=c(suturing1_score,suturing2_score,suturing3_score,suturing4_score,suturing5_score)

task_scores <- data.frame(CuttingScore=Cutting_Scores1, SuturingScore=Suturing_Scores1,row.names = c(1,2,3,4,5)) 

task.mat <- as.matrix(task_scores)[,1:2]

barplot(t(task.mat),beside=TRUE,main="Subject22",col=c("green","blue"),ylim=c(0,30),xlab="Session",ylab="Score")

#Subject14

#Session 1 

cut1_score<-performance_data$Score.Cut2
sut1_score<-performance_data$Score.Sut2

cutting1_score<-as.numeric(as.character(cut1_score[14]))
suturing1_score<-as.numeric(as.character(sut1_score[14]))

#Session 2

cut2_score<-performance_data$Score.Cut2.1
sut2_score<-performance_data$Score.Sut2.1

cutting2_score<-as.numeric(as.character(cut2_score[14]))
suturing2_score<-as.numeric(as.character(sut2_score[14]))

#Session 3

cut3_score<-performance_data$Score.Cut2.2
sut3_score<-performance_data$Score.Sut2.2

cutting3_score<-as.numeric(as.character(cut3_score[14]))
suturing3_score<-as.numeric(as.character(sut3_score[14]))

#Session 4

cut4_score<-performance_data$Score.Cut2.3
sut4_score<-performance_data$Score.Sut2.3

cutting4_score<-as.numeric(as.character(cut4_score[14]))
suturing4_score<-as.numeric(as.character(sut4_score[14]))

#Session 5

cut5_score<-performance_data$Score.Cut2.4
sut5_score<-performance_data$Score.Sut2.4

cutting5_score<-as.numeric(as.character(cut5_score[14]))
suturing5_score<-as.numeric(as.character(sut5_score[14]))

Cutting_Scores1 = c(cutting1_score,cutting2_score,cutting3_score,cutting4_score,cutting5_score)
Suturing_Scores1=c(suturing1_score,suturing2_score,suturing3_score,suturing4_score,suturing5_score)

task_scores <- data.frame(CuttingScore=Cutting_Scores1, SuturingScore=Suturing_Scores1,row.names = c(1,2,3,4,5)) 

task.mat <- as.matrix(task_scores)[,1:2]

barplot(t(task.mat),beside=TRUE,main="Subject24",col=c("green","blue"),ylim=c(0,30),xlab="Session")

#Subject15

#Session 1 

cut1_score<-performance_data$Score.Cut2
sut1_score<-performance_data$Score.Sut2

cutting1_score<-as.numeric(as.character(cut1_score[15]))
suturing1_score<-as.numeric(as.character(sut1_score[15]))

#Session 2

cut2_score<-performance_data$Score.Cut2.1
sut2_score<-performance_data$Score.Sut2.1

cutting2_score<-as.numeric(as.character(cut2_score[15]))
suturing2_score<-as.numeric(as.character(sut2_score[15]))

#Session 3

cut3_score<-performance_data$Score.Cut2.2
sut3_score<-performance_data$Score.Sut2.2

cutting3_score<-as.numeric(as.character(cut3_score[15]))
suturing3_score<-as.numeric(as.character(sut3_score[15]))

#Session 4

cut4_score<-performance_data$Score.Cut2.3
sut4_score<-performance_data$Score.Sut2.3

cutting4_score<-as.numeric(as.character(cut4_score[15]))
suturing4_score<-as.numeric(as.character(sut4_score[15]))

#Session 5

cut5_score<-performance_data$Score.Cut2.4
sut5_score<-performance_data$Score.Sut2.4

cutting5_score<-as.numeric(as.character(cut5_score[15]))
suturing5_score<-as.numeric(as.character(sut5_score[15]))

Cutting_Scores1 = c(cutting1_score,cutting2_score,cutting3_score,cutting4_score,cutting5_score)
Suturing_Scores1=c(suturing1_score,suturing2_score,suturing3_score,suturing4_score,suturing5_score)

task_scores <- data.frame(CuttingScore=Cutting_Scores1, SuturingScore=Suturing_Scores1,row.names = c(1,2,3,4,5)) 

task.mat <- as.matrix(task_scores)[,1:2]

barplot(t(task.mat),beside=TRUE,main="Subject26",col=c("green","blue"),ylim=c(0,30),xlab="Session")

title(main="Accuracy Bar Plot-Cutting Vs Suturing as per Scorer2",outer=T,xlab="Session",ylab="Score")

