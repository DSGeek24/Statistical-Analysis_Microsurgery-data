
############################################################################################################
##############################    INTERACTION TASK * SESSION    ############################################
############################################################################################################

##### Scorer #####
Score <- df_MicrosurgeryPerformance$Score
Task <- df_MicrosurgeryPerformance$Task
Session <- df_MicrosurgeryPerformance$Sessions

# AOV Model with Interaction between Task and Sessions
aov_InteractionModel_Scorer1 =  aov( Score ~  Task * Session)
  
# Summary of Analysis of Variance of Model with Interaction
summary(aov_InteractionModel_Scorer1)
coefficients(aov_InteractionModel_Scorer1)

# Regression Model with Interaction between Task and Sessions
regression_Interaction_Scorer1 = lm(Score ~  Task * Session)

# Summary of Regression Model with Interaction
summary(regression_Interaction_Scorer1)

