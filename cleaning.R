#load("raw_data_E3.Rda")

data$Condition <- NA

data <- data %>% 
    mutate(Subject = rep(1:50, each = 288)) %>%
    group_by(subID) %>%
    mutate(Phase = c(rep(1,144),rep(2,144)),
           block = c(rep(1,72),rep(2,72),rep(1,72),rep(2,72))
    )

data$ACC <- as.numeric(as.character(data$ACC))
data$trial <- as.numeric(as.character(data$trial))

data <- data %>%
    mutate(
        Frequency = case_when(trialProp == "0" ~ "biased",
                              trialProp == "100" ~ "biased",
                              trialProp == "50" ~ "unbiased"),
        Task_Relevant_Context = case_when(as.character(countProp) == "0" ~ "0% Con",
                                          as.character(countProp) == "100" ~ "100% Con"),
        order = case_when(order == "B1 0" ~ "0-100",
                          order == "B1 100" ~ "100-0")
    )


data <- data %>%
    rename(Trial = trial,
           Congruency = trialCongruency,
           Order = order,
           Block = block
           )

data <- data %>%
    ungroup() %>%
    select(
        Condition,
        Subject,
        Order,
        Phase,
        Block,
        Trial,
        Task_Relevant_Context,
        Frequency,
        Congruency,
        ACC,
        RT
    )

raw_data_E3 <- data
save(raw_data_E3, file="raw_data_E3.Rda")

load("demographics_E3.Rda")

questData$expTime<- ((as.numeric(as.character(questData$completeTime)) - as.numeric(as.character(questData$startTime)))/1000)/60
questData$Exp<- "E3"
questData <- questData %>%
    select(Exp,subnum,expTime,country,gender,age,handedness,vision,english,browser,count1_Cat,count1_Feat,count1_Prop,count2_Cat,count2_Feat,count2_Prop, task_feedback)

demographics_E3 <- questData
save(demographics_E3, file="demographics_E3.Rda")


#load("raw_data_E2.Rda")

data$Condition <- NA

data <- data %>% 
    mutate(Subject = rep(1:50, each = 288)) %>%
    group_by(subID) %>%
    mutate(Phase = c(rep(1,144),rep(2,144)),
           block = c(rep(1,72),rep(2,72),rep(1,72),rep(2,72)),
           PC = case_when(as.character(trialProp) == "25" ~ "25%",
                          as.character(trialProp) == "50" ~ "50%",
                          as.character(trialProp) == "75" ~ "75%")
    )

data$ACC <- as.numeric(as.character(data$ACC))
data$trial <- as.numeric(as.character(data$trial))


data <- data %>%
    mutate(
        Frequency = case_when(trialProp == "25" ~ "biased",
                              trialProp == "75" ~ "biased",
                              trialProp == "50" ~ "unbiased"),
        Task_Relevant_Context = NA,
        order = NA
    )

data$RT <- as.numeric(as.character(data$time2)) - as.numeric(as.character(data$time1))

data <- data %>%
    rename(Trial = trial,
           Congruency = trialCongruency,
           Order = order,
           Block = block
    )


data <- data %>%
    ungroup() %>%
    select(
        Condition,
        Subject,
        Order,
        Phase,
        Block,
        Trial,
        Task_Relevant_Context,
        PC,
        Frequency,
        Congruency,
        ACC,
        RT
    )

raw_data_E2 <- data
save(raw_data_E2, file="raw_data_E2.Rda")

#load("demographics_E2.Rda")

questData$expTime<- ((as.numeric(as.character(questData$completeTime)) - as.numeric(as.character(questData$startTime)))/1000)/60
questData$Exp<- "E2"
questData <- questData %>%
    select(Exp,subnum,expTime,country,gender,age,handedness,vision,english,browser,count1_Cat,count1_Feat,count1_Prop,count2_Cat,count2_Feat,count2_Prop)

demographics_E2 <- questData
save(demographics_E2, file="demographics_E2.Rda")

