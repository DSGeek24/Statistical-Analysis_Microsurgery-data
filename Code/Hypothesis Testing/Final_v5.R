setwd(
  "D:/Downloads/osfstorage-archive/Methodist microsurgery with output"
)
fp = "D:/Downloads/osfstorage-archive/Methodist microsurgery with output/"
list.files()
#################################################################################

#Finding the mean of each second
means <- data.frame()
downsampling <- function(filepath) {
  signaldata <- read.csv(filepath, sep = ",", header = T)
  min <- floor(min(signaldata$Time))
  max <- floor(max(signaldata$Time))
  for (i in min:max) {
    signaldata_mod <-
      signaldata[which(signaldata$Time >= i & signaldata$Time < (i + 1)), ]
    rows = nrow(signaldata_mod)
    Averageval = sum(signaldata_mod$Perspiration) / rows
    #means<- rbind(means, c(1, i,Averageval))
    #Averageval = log(Averageval)
    means <- rbind(means, c(1, i, Averageval))
    
  }
  colnames(means) <- c("Frames", "Time", "Perspiration")
  means_actual <- means
  means <-
    data.frame(Frames = integer(),
               Time = integer(),
               Perspiration = integer())
  return(means_actual)
}

#Finding the mean of a series of stress signals
findmean <- function(signals) {
  rows <- nrow(signals)
  grand_mean <- sum(signals$Perspiration, na.rm = T) / rows
  
  return(grand_mean)
}

#Final function - that computes mean per task per session
logpp <- function(filepath) {
  signalmeans <- downsampling(filepath)
  grandmean <- findmean(signalmeans)
  
  return(grandmean)
}

#################################################################################



MicrosurgeryPerformance = read.table(
  "MicrosurgeryPerformance.csv",
  header = TRUE,
  sep = ",",
  stringsAsFactors = FALSE
)

MicrosurgeryPerformance <- na.omit(MicrosurgeryPerformance)

colnames(MicrosurgeryPerformance) <-
  c(
    "Subject_ID",
    "Age",
    "Ms-Year",
    "Sex",
    "Cutting Time S1",
    "Suturing Time S1",
    "Sutures S1",
    "Scorer1 Cutting S1",
    "Scorer1 Suturing S1",
    "Scorer2 Cutting S1"	,
    "Scorer2 Suturing S1",
    "Cutting Time S2",
    "Suturing Time S2",
    "Sutures S2",
    "Scorer1 Cutting S2"	,
    "Scorer1 Suturing S2",
    "Scorer2 Cutting S2",
    "Scorer2 Suturing S2",
    "Cutting Time S3",
    "Suturing Time S3",
    "Sutures S3",
    "Scorer1 Cutting S3",
    "Scorer1 Suturing S3",
    "Scorer2 Cutting S3",
    "Scorer2 Suturing S3",
    "Cutting Time S4",
    "Suturing Time S4"	,
    "Sutures S4"	,
    "Scorer1 Cutting S4"	,
    "Scorer1 Suturing S4",
    "Scorer2 Cutting S4"	,
    "Scorer2 Suturing S4",
    "Cutting Time S5",
    "Suturing Time S5"	,
    "Sutures S5",
    "Scorer1 Cutting S5",
    "Scorer1 Suturing S5",
    "Scorer2 Cutting S5",
    "Scorer2 Suturing S5"
  )

#################################################################################
No_of_iterations = length(MicrosurgeryPerformance$Subject_ID) * 10

df_MicrosurgeryPerformance <-
  data.frame(matrix(ncol = 10, nrow = No_of_iterations))

colnames(df_MicrosurgeryPerformance) <-
  c(
    "Subjects",
    "Sessions",
    "Task",
    "Sex",
    "Age",
    "Average_Perspiration",
    "Score",
    "Scorer",
    "time",
    "NoSutures"
  )

No_Of_Rows = 1

filepath = ""
MicrosurgeryPerformance$`Sutures S2`[2]
for (i in 1:length(MicrosurgeryPerformance$Subject_ID)) {
  #############  Cutting - Scorer 1 ###############
  
  for (j in 1:5) {
    Subject_ID = MicrosurgeryPerformance$Subject_ID[i]
    Session = j
    Task = "Cutting"
    Scorer = "1"
    
    if (MicrosurgeryPerformance$Sex[i] == 1) {
      Sex = "Male"
    }
    else{
      Sex = "Female"
    }
   
    Age = MicrosurgeryPerformance$Age[i]
    
    if (j == 1) {
      time = MicrosurgeryPerformance$`Cutting Time S1`[i]
      NoSutures = NA
      if (Subject_ID < 10) {
        id = paste("0", Subject_ID, sep = "")
      }
      else{
        id = Subject_ID
      }
      
      filepath = paste("subject",
                       id,
                       "/session",
                       j,
                       "/Subject",
                       id,
                       "_Cutting1.csv",
                       sep = "")
      temp = paste(fp, filepath, sep = "")
      if (file.exists(temp)) {
        Average_PP = logpp(filepath)
        filepath = paste("subject",
                         id,
                         "/session",
                         j,
                         "/Subject",
                         id,
                         "_Baseline1.csv",
                         sep = "")
        temp = paste(fp, filepath, sep = "")
        if (file.exists(temp)) {
          Base_pp = logpp(filepath)
          Average_PP = Average_PP - Base_pp
        }
        else{
          Average_PP = NA
        }
        
      }
      else{
        Average_PP = NA
      }
      
      
      Score =  as.numeric(MicrosurgeryPerformance$`Scorer1 Cutting S1`[i])
    }
    else if (j == 2) {
      time = MicrosurgeryPerformance$`Cutting Time S2`[i]
      NoSutures = NA
      if (Subject_ID < 10) {
        id = paste("0", Subject_ID, sep = "")
      }
      
      else{
        id = Subject_ID
      }
      filepath = paste("subject",
                       id,
                       "/session",
                       j,
                       "/Subject",
                       id,
                       "_Cutting2.csv",
                       sep = "")
      
      if (file.exists(filepath)) {
        Average_PP = logpp(filepath)
        filepath = paste("subject",
                         id,
                         "/session",
                         j,
                         "/Subject",
                         id,
                         "_Baseline2.csv",
                         sep = "")
        temp = paste(fp, filepath, sep = "")
        if (file.exists(temp)) {
          Base_pp = logpp(filepath)
          Average_PP = Average_PP - Base_pp
        }
        else{
          Average_PP = NA
        }
      }
      else{
        Average_PP = NA
      }
      Score = as.numeric(MicrosurgeryPerformance$`Scorer1 Cutting S2`[i])
    }
    else if (j == 3) {
      time = MicrosurgeryPerformance$`Cutting Time S3`[i]
      NoSutures = NA
      if (Subject_ID < 10) {
        id = paste("0", Subject_ID, sep = "")
      }
      else{
        id = Subject_ID
      }
      filepath = paste("subject",
                       id,
                       "/session",
                       j,
                       "/Subject",
                       id,
                       "_Cutting3.csv",
                       sep = "")
      
      if (file.exists(filepath)) {
        Average_PP = logpp(filepath)
        filepath = paste("subject",
                         id,
                         "/session",
                         j,
                         "/Subject",
                         id,
                         "_Baseline3.csv",
                         sep = "")
        temp = paste(fp, filepath, sep = "")
        if (file.exists(temp)) {
          Base_pp = logpp(filepath)
          Average_PP = Average_PP - Base_pp
        }
        else{
          Average_PP = NA
        }
        
      }
      else{
        Average_PP = NA
      }
      Score = as.numeric(MicrosurgeryPerformance$`Scorer1 Cutting S3`[i])
    }
    else if (j == 4) {
      time = MicrosurgeryPerformance$`Cutting Time S4`[i]
      NoSutures = NA
      if (Subject_ID < 10) {
        id = paste("0", Subject_ID, sep = "")
      }
      else{
        id = Subject_ID
      }
      filepath = paste("subject",
                       id,
                       "/session",
                       j,
                       "/Subject",
                       id,
                       "_Cutting4.csv",
                       sep = "")
      
      if (file.exists(filepath)) {
        Average_PP = logpp(filepath)
        filepath = paste("subject",
                         id,
                         "/session",
                         j,
                         "/Subject",
                         id,
                         "_Baseline4.csv",
                         sep = "")
        temp = paste(fp, filepath, sep = "")
        if (file.exists(temp)) {
          Base_pp = logpp(filepath)
          Average_PP = Average_PP - Base_pp
        }
        else{
          Average_PP = NA
        }
        
      }
      else{
        Average_PP = NA
      }
      Score = as.numeric(MicrosurgeryPerformance$`Scorer1 Cutting S4`[i])
    }
    else {
      time = MicrosurgeryPerformance$`Cutting Time S5`[i]
      NoSutures = NA
      if (Subject_ID < 10) {
        id = paste("0", Subject_ID, sep = "")
      }
      else{
        id = Subject_ID
      }
      filepath = paste("subject",
                       id,
                       "/session",
                       j,
                       "/Subject",
                       id,
                       "_Cutting5.csv",
                       sep = "")
      
      if (file.exists(filepath)) {
        Average_PP = logpp(filepath)
        filepath = paste("subject",
                         id,
                         "/session",
                         j,
                         "/Subject",
                         id,
                         "_Baseline5.csv",
                         sep = "")
        temp = paste(fp, filepath, sep = "")
        if (file.exists(temp)) {
          Base_pp = logpp(filepath)
          Average_PP = Average_PP - Base_pp
        }
        else{
          Average_PP = NA
        }
        
      }
      else{
        Average_PP = NA
      }
      Score = as.numeric(MicrosurgeryPerformance$`Scorer1 Cutting S5`[i])
    }
    
    df_MicrosurgeryPerformance[No_Of_Rows,] <-
      c(Subject_ID,
        Session,
        Task,
        Sex,
        Age,
        Average_PP,
        Score,
        Scorer,time,NoSutures)
    No_Of_Rows = No_Of_Rows + 1
  }
  
  #############  Cutting - Scorer 2 ###############
  
  for (j in 1:5) {
    Subject_ID = MicrosurgeryPerformance$Subject_ID[i]
    Session = j
    Task = "Cutting"
    Scorer = "2"
   
    if (MicrosurgeryPerformance$Sex[i] == 1) {
      Sex = "Male"
    }
    else{
      Sex = "Female"
    }
    Age = MicrosurgeryPerformance$Age[i]
    
    if (j == 1) {
      time = MicrosurgeryPerformance$`Cutting Time S1`[i]
      NoSutures = NA
      if (Subject_ID < 10) {
        id = paste("0", Subject_ID, sep = "")
      }
      else{
        id = Subject_ID
      }
      filepath = paste("subject",
                       id,
                       "/session",
                       j,
                       "/Subject",
                       id,
                       "_Cutting1.csv",
                       sep = "")
      
      if (file.exists(filepath)) {
        Average_PP = logpp(filepath)
        filepath = paste("subject",
                         id,
                         "/session",
                         j,
                         "/Subject",
                         id,
                         "_Baseline1.csv",
                         sep = "")
        temp = paste(fp, filepath, sep = "")
        if (file.exists(temp)) {
          Base_pp = logpp(filepath)
          Average_PP = Average_PP - Base_pp
        }
        else{
          Average_PP = NA
        }
        
      }
      else{
        Average_PP = NA
      }
      Score =  as.numeric(MicrosurgeryPerformance$`Scorer2 Cutting S1`[i])
    }
    else if (j == 2) {
      time = MicrosurgeryPerformance$`Cutting Time S2`[i]
      NoSutures = NA
      if (Subject_ID < 10) {
        id = paste("0", Subject_ID, sep = "")
      }
      else{
        id = Subject_ID
      }
      filepath = paste("subject",
                       id,
                       "/session",
                       j,
                       "/Subject",
                       id,
                       "_Cutting2.csv",
                       sep = "")
      
      if (file.exists(filepath)) {
        Average_PP = logpp(filepath)
        filepath = paste("subject",
                         id,
                         "/session",
                         j,
                         "/Subject",
                         id,
                         "_Baseline2.csv",
                         sep = "")
        temp = paste(fp, filepath, sep = "")
        if (file.exists(temp)) {
          Base_pp = logpp(filepath)
          Average_PP = Average_PP - Base_pp
        }
        else{
          Average_PP = NA
        }
        
      }
      else{
        Average_PP = NA
      }
      Score = as.numeric(MicrosurgeryPerformance$`Scorer2 Cutting S2`[i])
    }
    else if (j == 3) {
      time = MicrosurgeryPerformance$`Cutting Time S3`[i]
      NoSutures = NA
      if (Subject_ID < 10) {
        id = paste("0", Subject_ID, sep = "")
      }
      else{
        id = Subject_ID
      }
      filepath = paste("subject",
                       id,
                       "/session",
                       j,
                       "/Subject",
                       id,
                       "_Cutting3.csv",
                       sep = "")
      
      if (file.exists(filepath)) {
        Average_PP = logpp(filepath)
        filepath = paste("subject",
                         id,
                         "/session",
                         j,
                         "/Subject",
                         id,
                         "_Baseline3.csv",
                         sep = "")
        temp = paste(fp, filepath, sep = "")
        if (file.exists(temp)) {
          Base_pp = logpp(filepath)
          Average_PP = Average_PP - Base_pp
        }
        else{
          Average_PP = NA
        }
        
      }
      else{
        Average_PP = NA
      }
      Score = as.numeric(MicrosurgeryPerformance$`Scorer2 Cutting S3`[i])
    }
    else if (j == 4) {
      time = MicrosurgeryPerformance$`Cutting Time S4`[i]
      NoSutures = NA
      if (Subject_ID < 10) {
        id = paste("0", Subject_ID, sep = "")
      }
      else{
        id = Subject_ID
      }
      filepath = paste("subject",
                       id,
                       "/session",
                       j,
                       "/Subject",
                       id,
                       "_Cutting4.csv",
                       sep = "")
      
      if (file.exists(filepath)) {
        Average_PP = logpp(filepath)
        filepath = paste("subject",
                         id,
                         "/session",
                         j,
                         "/Subject",
                         id,
                         "_Baseline4.csv",
                         sep = "")
        temp = paste(fp, filepath, sep = "")
        if (file.exists(temp)) {
          Base_pp = logpp(filepath)
          Average_PP = Average_PP - Base_pp
        }
        else{
          Average_PP = NA
        }
        
      }
      else{
        Average_PP = NA
      }
      Score = as.numeric(MicrosurgeryPerformance$`Scorer2 Cutting S4`[i])
    }
    else {
      time = MicrosurgeryPerformance$`Cutting Time S5`[i]
      NoSutures = NA
      if (Subject_ID < 10) {
        id = paste("0", Subject_ID, sep = "")
      }
      else{
        id = Subject_ID
      }
      filepath = paste("subject",
                       id,
                       "/session",
                       j,
                       "/Subject",
                       id,
                       "_Cutting5.csv",
                       sep = "")
      
      if (file.exists(filepath)) {
        Average_PP = logpp(filepath)
        filepath = paste("subject",
                         id,
                         "/session",
                         j,
                         "/Subject",
                         id,
                         "_Baseline5.csv",
                         sep = "")
        temp = paste(fp, filepath, sep = "")
        if (file.exists(temp)) {
          Base_pp = logpp(filepath)
          Average_PP = Average_PP - Base_pp
        }
        else{
          Average_PP = NA
        }
      }
      else{
        Average_PP = NA
      }
      Score = as.numeric(MicrosurgeryPerformance$`Scorer2 Cutting S5`[i])
    }
    
    df_MicrosurgeryPerformance[No_Of_Rows,] <-
      c(Subject_ID,
        Session,
        Task,
        Sex,
        Age,
        Average_PP,
        Score,
        Scorer,time,NoSutures)
    No_Of_Rows = No_Of_Rows + 1
  }
  
  
  #############  Suturing - Scorer 1 ###############
  
  for (j in 1:5) {
    Subject_ID = MicrosurgeryPerformance$Subject_ID[i]
    Session = j
    Task = "Suturing"
    Scorer = "1"
    if (MicrosurgeryPerformance$Sex[i] == 1) {
      Sex = "Male"
    }
    else{
      Sex = "Female"
    }
    Age = MicrosurgeryPerformance$Age[i]
    
    if (j == 1) {
      time = MicrosurgeryPerformance$`Suturing Time S1`[i]
      NoSutures = MicrosurgeryPerformance$`Sutures S1`[i]
      if (Subject_ID < 10) {
        id = paste("0", Subject_ID, sep = "")
      }
      else{
        id = Subject_ID
      }
      filepath = paste("subject",
                       id,
                       "/session",
                       j,
                       "/Subject",
                       id,
                       "_Suturing1.csv",
                       sep = "")
      
      if (file.exists(filepath)) {
        Average_PP = logpp(filepath)
        filepath = paste("subject",
                         id,
                         "/session",
                         j,
                         "/Subject",
                         id,
                         "_Baseline1.csv",
                         sep = "")
        temp = paste(fp, filepath, sep = "")
        if (file.exists(temp)) {
          Base_pp = logpp(filepath)
          Average_PP = Average_PP - Base_pp
        }
        else{
          Average_PP = NA
        }
        
      }
      else{
        Average_PP = NA
      }
      Score = as.numeric(MicrosurgeryPerformance$`Scorer1 Suturing S1`[i]) #+ as.numeric(MicrosurgeryPerformance$`Scorer2 Suturing S1`[i])) / 2
    }
    else if (j == 2) {
      time = MicrosurgeryPerformance$`Suturing Time S2`[i]
      NoSutures = MicrosurgeryPerformance$`Sutures S2`[i]
      if (Subject_ID < 10) {
        id = paste("0", Subject_ID, sep = "")
      }
      else{
        id = Subject_ID
      }
      filepath = paste("subject",
                       id,
                       "/session",
                       j,
                       "/Subject",
                       id,
                       "_Suturing2.csv",
                       sep = "")
      
      if (file.exists(filepath)) {
        Average_PP = logpp(filepath)
        filepath = paste("subject",
                         id,
                         "/session",
                         j,
                         "/Subject",
                         id,
                         "_Baseline2.csv",
                         sep = "")
        temp = paste(fp, filepath, sep = "")
        if (file.exists(temp)) {
          Base_pp = logpp(filepath)
          Average_PP = Average_PP - Base_pp
        }
        else{
          Average_PP = NA
        }
      }
      else{
        Average_PP = NA
      }
      Score =  as.numeric(MicrosurgeryPerformance$`Scorer1 Suturing S2`[i]) #+ as.numeric(MicrosurgeryPerformance$`Scorer2 Suturing S2`[i])) / 2
    }
    else if (j == 3) {
      time = MicrosurgeryPerformance$`Suturing Time S3`[i]
      NoSutures = MicrosurgeryPerformance$`Sutures S3`[i]
      if (Subject_ID < 10) {
        id = paste("0", Subject_ID, sep = "")
      }
      else{
        id = Subject_ID
      }
      filepath = paste("subject",
                       id,
                       "/session",
                       j,
                       "/Subject",
                       id,
                       "_Suturing3.csv",
                       sep = "")
      
      if (file.exists(filepath)) {
        Average_PP = logpp(filepath)
        filepath = paste("subject",
                         id,
                         "/session",
                         j,
                         "/Subject",
                         id,
                         "_Baseline3.csv",
                         sep = "")
        temp = paste(fp, filepath, sep = "")
        if (file.exists(temp)) {
          Base_pp = logpp(filepath)
          Average_PP = Average_PP - Base_pp
        }
        else{
          Average_PP = NA
        }
      }
      else{
        Average_PP = NA
      }
      Score =  as.numeric(MicrosurgeryPerformance$`Scorer1 Suturing S3`[i]) #+ as.numeric(MicrosurgeryPerformance$`Scorer2 Suturing S3`[i])) / 2
    }
    else if (j == 4) {
      time = MicrosurgeryPerformance$`Suturing Time S4`[i]
      NoSutures = MicrosurgeryPerformance$`Sutures S4`[i]
      if (Subject_ID < 10) {
        id = paste("0", Subject_ID, sep = "")
      }
      else{
        id = Subject_ID
      }
      filepath = paste("subject",
                       id,
                       "/session",
                       j,
                       "/Subject",
                       id,
                       "_Suturing4.csv",
                       sep = "")
      
      if (file.exists(filepath)) {
        Average_PP = logpp(filepath)
        filepath = paste("subject",
                         id,
                         "/session",
                         j,
                         "/Subject",
                         id,
                         "_Baseline4.csv",
                         sep = "")
        temp = paste(fp, filepath, sep = "")
        if (file.exists(temp)) {
          Base_pp = logpp(filepath)
          Average_PP = Average_PP - Base_pp
        }
        else{
          Average_PP = NA
        }
      }
      else{
        Average_PP = NA
      }
      Score =  as.numeric(MicrosurgeryPerformance$`Scorer1 Suturing S4`[i]) #+ as.numeric(MicrosurgeryPerformance$`Scorer2 Suturing S4`[i])) / 2
    }
    else {
      time = MicrosurgeryPerformance$`Suturing Time S5`[i]
      NoSutures = MicrosurgeryPerformance$`Sutures S5`[i]
      if (Subject_ID < 10) {
        id = paste("0", Subject_ID, sep = "")
      }
      else{
        id = Subject_ID
      }
      filepath = paste("subject",
                       id,
                       "/session",
                       j,
                       "/Subject",
                       id,
                       "_Suturing5.csv",
                       sep = "")
      
      if (file.exists(filepath)) {
        Average_PP = logpp(filepath)
        filepath = paste("subject",
                         id,
                         "/session",
                         j,
                         "/Subject",
                         id,
                         "_Baseline5.csv",
                         sep = "")
        temp = paste(fp, filepath, sep = "")
        if (file.exists(temp)) {
          Base_pp = logpp(filepath)
          Average_PP = Average_PP - Base_pp
        }
        else{
          Average_PP = NA
        }
      }
      else{
        Average_PP = NA
      }
      Score = as.numeric(MicrosurgeryPerformance$`Scorer1 Suturing S5`[i]) #+ as.numeric(MicrosurgeryPerformance$`Scorer2 Suturing S5`[i])) / 2
    }
    
    df_MicrosurgeryPerformance[No_Of_Rows,] <-
      c(Subject_ID,
        Session,
        Task,
        Sex,
        Age,
        Average_PP,
        Score,
        Scorer,time,NoSutures)
    No_Of_Rows = No_Of_Rows + 1
  }
  
  #############  Suturing - Scorer 2 ###############
  for (j in 1:5) {
    Subject_ID = MicrosurgeryPerformance$Subject_ID[i]
    Session = j
    Task = "Suturing"
    Scorer = "2"
    if (MicrosurgeryPerformance$Sex[i] == 1) {
      Sex = "Male"
    }
    else{
      Sex = "Female"
    }
    Age = MicrosurgeryPerformance$Age[i]
    
    if (j == 1) {
      time = MicrosurgeryPerformance$`Suturing Time S1`[i]
      NoSutures = MicrosurgeryPerformance$`Sutures S1`[i]
      if (Subject_ID < 10) {
        id = paste("0", Subject_ID, sep = "")
      }
      else{
        id = Subject_ID
      }
      filepath = paste("subject",
                       id,
                       "/session",
                       j,
                       "/Subject",
                       id,
                       "_Suturing1.csv",
                       sep = "")
      
      if (file.exists(filepath)) {
        Average_PP = logpp(filepath)
        filepath = paste("subject",
                         id,
                         "/session",
                         j,
                         "/Subject",
                         id,
                         "_Baseline1.csv",
                         sep = "")
        temp = paste(fp, filepath, sep = "")
        if (file.exists(temp)) {
          Base_pp = logpp(filepath)
          Average_PP = Average_PP - Base_pp
        }
        else{
          Average_PP = NA
        }
      }
      else{
        Average_PP = NA
      }
      Score = as.numeric(MicrosurgeryPerformance$`Scorer2 Suturing S1`[i])
    }
    else if (j == 2) {
      time = MicrosurgeryPerformance$`Suturing Time S2`[i]
      NoSutures = MicrosurgeryPerformance$`Sutures S2`[i]
      if (Subject_ID < 10) {
        id = paste("0", Subject_ID, sep = "")
      }
      else{
        id = Subject_ID
      }
      filepath = paste("subject",
                       id,
                       "/session",
                       j,
                       "/Subject",
                       id,
                       "_Suturing2.csv",
                       sep = "")
      
      if (file.exists(filepath)) {
        Average_PP = logpp(filepath)
        filepath = paste("subject",
                         id,
                         "/session",
                         j,
                         "/Subject",
                         id,
                         "_Baseline2.csv",
                         sep = "")
        temp = paste(fp, filepath, sep = "")
        if (file.exists(temp)) {
          Base_pp = logpp(filepath)
          Average_PP = Average_PP - Base_pp
        }
        else{
          Average_PP = NA
        }
      }
      else{
        Average_PP = NA
      }
      Score =  as.numeric(MicrosurgeryPerformance$`Scorer2 Suturing S2`[i])
    }
    else if (j == 3) {
      time = MicrosurgeryPerformance$`Suturing Time S3`[i]
      NoSutures = MicrosurgeryPerformance$`Sutures S3`[i]
      if (Subject_ID < 10) {
        id = paste("0", Subject_ID, sep = "")
      }
      else{
        id = Subject_ID
      }
      filepath = paste("subject",
                       id,
                       "/session",
                       j,
                       "/Subject",
                       id,
                       "_Suturing3.csv",
                       sep = "")
      
      if (file.exists(filepath)) {
        Average_PP = logpp(filepath)
        filepath = paste("subject",
                         id,
                         "/session",
                         j,
                         "/Subject",
                         id,
                         "_Baseline3.csv",
                         sep = "")
        temp = paste(fp, filepath, sep = "")
        if (file.exists(temp)) {
          Base_pp = logpp(filepath)
          Average_PP = Average_PP - Base_pp
        }
        else{
          Average_PP = NA
        }
      }
      else{
        Average_PP = NA
      }
      Score =  as.numeric(MicrosurgeryPerformance$`Scorer2 Suturing S3`[i])
    }
    else if (j == 4) {
      time = MicrosurgeryPerformance$`Suturing Time S4`[i]
      NoSutures = MicrosurgeryPerformance$`Sutures S4`[i]
      if (Subject_ID < 10) {
        id = paste("0", Subject_ID, sep = "")
        
      }
      
      else{
        id = Subject_ID
      }
      filepath = paste("subject",
                       id,
                       "/session",
                       j,
                       "/Subject",
                       id,
                       "_Suturing4.csv",
                       sep = "")
      
      if (file.exists(filepath)) {
        Average_PP = logpp(filepath)
        filepath = paste("subject",
                         id,
                         "/session",
                         j,
                         "/Subject",
                         id,
                         "_Baseline4.csv",
                         sep = "")
        temp = paste(fp, filepath, sep = "")
        if (file.exists(temp)) {
          Base_pp = logpp(filepath)
          Average_PP = Average_PP - Base_pp
        }
        else{
          Average_PP = NA
        }
      }
      else{
        Average_PP = NA
      }
      Score =  as.numeric(MicrosurgeryPerformance$`Scorer2 Suturing S4`[i])
    }
    
    else {
      time = MicrosurgeryPerformance$`Suturing Time S5`[i]
      NoSutures = MicrosurgeryPerformance$`Sutures S5`[i]
      if (Subject_ID < 10) {
        id = paste("0", Subject_ID, sep = "")
      }
      else{
        id = Subject_ID
      }
      filepath = paste("subject",
                       id,
                       "/session",
                       j,
                       "/Subject",
                       id,
                       "_Suturing5.csv",
                       sep = "")
      if (file.exists(filepath)) {
        Average_PP = logpp(filepath)
        filepath = paste("subject",
                         id,
                         "/session",
                         j,
                         "/Subject",
                         id,
                         "_Baseline5.csv",
                         sep = "")
        temp = paste(fp, filepath, sep = "")
        if (file.exists(temp)) {
          Base_pp = logpp(filepath)
          Average_PP = Average_PP - Base_pp
        }
        else{
          Average_PP = NA
        }
      }
      else{
        Average_PP = NA
      }
      Score = as.numeric(MicrosurgeryPerformance$`Scorer2 Suturing S5`[i])
    }
    df_MicrosurgeryPerformance[No_Of_Rows,] <-
      c(Subject_ID,
        Session,
        Task,
        Sex,
        Age,
        Average_PP,
        Score,
        Scorer,time,NoSutures)
    No_Of_Rows = No_Of_Rows + 1
  }
}
df_MicrosurgeryPerformance

###################################################################################################################

#Performing linear regression
df_MicrosurgeryPerformance$Average_Perspiration <- as.numeric(as.character(df_MicrosurgeryPerformance$Average_Perspiration))
df_MicrosurgeryPerformance$Age <- as.numeric(as.character(df_MicrosurgeryPerformance$Age))
df_MicrosurgeryPerformance$Score <- as.numeric(as.character(df_MicrosurgeryPerformance$Score))
min_pp = min(df_MicrosurgeryPerformance$Average_Perspiration, na.rm = T)
new_pp = df_MicrosurgeryPerformance$Average_Perspiration + abs(min_pp)
log_val_pp = log(new_pp)
df_MicrosurgeryPerformance$PP = log_val_pp
#Has to be run only once
#Making Inf to NA for corrplot
df_MicrosurgeryPerformance$PP[5] = NA
df_MicrosurgeryPerformance$PP[10] = NA
#########################################################   MODEL - Score #################################################################
sum1 <-
  lm(
    Score ~  PP  + Sessions + Task + Age + Sex  + Scorer ,
    data = df_MicrosurgeryPerformance
  )
summary(sum1)
coefficients(sum1)
shapiro.test(sum1$residuals)
#######################################################################################################################################
#########################################################   RANDOM    #################################################################
install.packages("nlme")
library(nlme)
sum2 <-
  lmer(
    Score ~  PP  + Sessions + Task + Age + Sex  + Scorer + (1|Subjects),
    data = df_MicrosurgeryPerformance
  )
summary(sum2)
coefficients(sum2)
plot(ranef(sum2),main="Distribution of Mixed effects model")

cor(df_MicrosurgeryPerformance$Score[df_MicrosurgeryPerformance$Scorer=="1"],df_MicrosurgeryPerformance$Score[df_MicrosurgeryPerformance$Scorer=="2"])


#######################################################################################################################################
#########################################################   MODEL - Time #################################################################
head(df_MicrosurgeryPerformance)
install.packages("lubridate")
library(lubridate)
period_to_seconds(ms("12:54"))
dat1 <- period_to_seconds(ms(df_MicrosurgeryPerformance$time))
df_MicrosurgeryPerformance$Time <- dat1
sum3 <-
  lm(
    Score ~  PP  + Sessions + Task + Age + Sex  + Scorer +Time,
    data = df_MicrosurgeryPerformance
  )
summary(sum3)
coefficients(sum3)
#########################################################   MODEL - Number of Sutures #################################################################
sapply(df_MicrosurgeryPerformance, class)

df_MicrosurgeryPerformance$NoSutures <- as.numeric(as.character(df_MicrosurgeryPerformance$NoSutures))
is.factor
sum4 <-
  lm(
    Score ~  PP  + Sessions  + Age + Sex  + Scorer+ NoSutures,
    data = df_MicrosurgeryPerformance
  )
#typeof(as.numeric(df_MicrosurgeryPerformance$NoSutures,na.rm=T))
summary(sum4)
coefficients(sum4)
#######################################################################################################################################

plot(sum, which = 4)
cor.test(df_MicrosurgeryPerformance$Sco)
sum1<-
aov(
  Score ~  Average_Perspiration  + Sessions + Task + Age + Sex  + Scorer ,
  data = df_MicrosurgeryPerformance
)
summary(sum1)
coefficients(sum1)
#######################################################################################################################################
################################################################################################3
#############CORRELATION PLOT#####################################################################
MPdat<- df_MicrosurgeryPerformance
MPdat$Task <- NULL
MPdat$Sex <- NULL
MPdat[is.na(MPdat)] <- 0
cor(MPdat, use="pairwise.complete.obs")
sapply(MPdat, class)
MPdat$Subjects <- as.numeric(as.character(MPdat$Subjects))
MPdat$Sessions <- as.numeric(as.character(MPdat$Sessions))
MPdat$Scorer <- as.numeric(as.character(MPdat$Scorer))
MPdat$time <- as.numeric(as.character(MPdat$time))
MPdat$time <- NULL
MPdat$num_Sutures <- as.numeric(as.character(MPdat$num_Sutures))
MPdat$NoSutures <-MPdat$num_Sutures
MPdat$num_Sutures <- NULL
library(corrplot)
M <- cor(MPdat)

corrplot(M,method = "circle")
pairs(MPdat)

lm()
corrplot()
#######################################################################################################################################
#########################INTERACTION PLOTS#######################################################################3
Time <- df_MicrosurgeryPerformance$Time
Task <- df_MicrosurgeryPerformance$Task
Session <- df_MicrosurgeryPerformance$Sessions


df_time = data.frame(Time, Task, Session)    

colnames(df_time) <- c("Time", "Task", "Session")

Tasks <- df_time$Task

interaction.plot(x.factor = df_time$Session, trace.factor = Tasks, 
                 response = df_time$Time, 
                 fun = mean,main="Time : Interaction Plot between Tasks and Sessions",
                 type="b", legend = TRUE,
                 col=c("black","red"),  
                 pch=c(19, 17),             
                 fixed=TRUE,                  
                 leg.bty = "o", xlab = "Sessions" ,ylab = "Time(secs)")

plot(sum1,which=1)

plot(df_MicrosurgeryPerformance$Average_Perspiration[df_MicrosurgeryPerformance$Task=="Cutting"],df_MicrosurgeryPerformance$Score[df_MicrosurgeryPerformance$Task=="Cutting"], main="ulation", xlab="Average PP", ylab="Score", pch=19,col =c("green"))
par(new=T)

plot(df_MicrosurgeryPerformance$Average_Perspiration[df_MicrosurgeryPerformance$Task=="Suturing"],df_MicrosurgeryPerformance$Score[df_MicrosurgeryPerformance$Task=="Suturing"], axes=F,  pch=18 ,xlab = " ",ylab = " ",col=c("blue"))

############################################################################################################
##############################    INTERACTION TASK * SESSION    ############################################
############################################################################################################

##### Scorer1 #####
Score <- df_MicrosurgeryPerformance$Score[df_MicrosurgeryPerformance$Scorer == "1"]
Task <- df_MicrosurgeryPerformance$Task[df_MicrosurgeryPerformance$Scorer == "1"]
Session <- df_MicrosurgeryPerformance$Sessions[df_MicrosurgeryPerformance$Scorer == "1"]

# AOV Model with Interaction between Task and Sessions
aov_InteractionModel_Scorer1 =  aov( Score ~  Task * Session)

# Summary of Analysis of Variance of Model with Interaction
summary(aov_InteractionModel_Scorer1)
coefficients(aov_InteractionModel_Scorer1)

# Regression Model with Interaction between Task and Sessions
regression_Interaction_Scorer1 = lm(Score ~  Task * Session)

# Summary of Regression Model with Interaction
summary(regression_Interaction_Scorer1)


##### Scorer2 #####
Score <- df_MicrosurgeryPerformance$Score[df_MicrosurgeryPerformance$Scorer == "2"]
Task <- df_MicrosurgeryPerformance$Task[df_MicrosurgeryPerformance$Scorer == "2"]
Session <- df_MicrosurgeryPerformance$Sessions[df_MicrosurgeryPerformance$Scorer == "2"]

# AOV Model with Interaction between Task and Sessions
aov_InteractionModel_Scorer2 =  aov( Score ~  Task * Session)

# Summary of Analysis of Variance of Model with Interaction
summary(aov_InteractionModel_Scorer2)
coefficients(aov_InteractionModel_Scorer2)

# Regression Model with Interaction between Task and Sessions
regression_Interaction_Scorer2 = lm(Score ~  Task * Session)

# Summary of Regression Model with Interaction
summary(regression_Interaction_Scorer2)


