
#######################################################################################################################################
##### Scorer  --- Interaction Plot  #####
#######################################################################################################################################
Score <- as.numeric(df_MicrosurgeryPerformance$Score)
Task <- df_MicrosurgeryPerformance$Task
Session <- df_MicrosurgeryPerformance$Sessions


df_scorer1 = data.frame(Score, Task, Session)    

colnames(df_scorer1) <- c("Score", "Task", "Session")

Tasks <- df_scorer1$Task

interaction.plot(x.factor = df_scorer1$Session, trace.factor = Tasks, 
                 response = df_scorer1$Score, 
                 fun = mean,main="Interaction Plot between Tasks and Sessions",
                 type="b", legend = TRUE,
                 col=c("black","red"),  ### Colors for levels of trace var.
                 pch=c(19, 17),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o",ylim = c(0,25), xlab = "Sessions" ,ylab = "Scorer 1 - Score")
  
