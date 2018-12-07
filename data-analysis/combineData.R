pack<-c("dplyr","ggplot2","pander","knitr","xtable","car","Crump","lsr","afex","stringr","tidyr")
lapply(pack, require, character.only = TRUE)


## LOAD DATA FROM E1 and E2 ###
load("EAll.Rda")
load("EAllq.Rda")

### ANALYZE QUIZ DATA ###
quizAnalysis <-tbl_df(data)
quizAnalysis <- quizAnalysis %>%
  filter(trialColor == "quiz")

quizAnalysis <- quizAnalysis[,-10:-17]
colnames(quizAnalysis)<- c("subID","trial","trialType","correctResponse","time1","time2","response","ACC","unknown","Subject","block","subBlock","subBlockB","newACC","preCon","preTrialProp", "order")

quizAnalysis <- quizAnalysis %>%
  filter(is.na(response) == FALSE)

quizAnalysis$response<-as.numeric(as.character(quizAnalysis$response))
quizAnalysis$correctResponse<-as.numeric(as.character(quizAnalysis$correctResponse))
quizAnalysis$ACC<-as.numeric(as.character(quizAnalysis$ACC))
quizAnalysis$fineACC<-abs(quizAnalysis$response - quizAnalysis$correctResponse)
quizAnalysis$Exp <- NA
quizAnalysis[quizAnalysis$Subject < 51,]$Exp <- "E1"
quizAnalysis[quizAnalysis$Subject > 51,]$Exp <- "E2"


quiz.Sum<- quizAnalysis %>%
  group_by(Subject) %>%
  filter(!is.na(fineACC),Subject != 4, Subject != 29) %>%
  summarise(ACC = mean(ACC), fineACC = mean(fineACC)) 

quiz.Sum
### END QUIZ DATA ###

### GET QUESTIONNAIRE DATA ###
questData$expTime<-((as.numeric(as.character(questData$completeTime)) -as.numeric(as.character(questData$startTime)))/1000)/60
questData$Exp<-NA
questData[questData$subnum < 51,]$Exp <- "E1"
questData[questData$subnum > 51,]$Exp <- "E2"
questData <- questData %>%
  select(Exp,subnum,expTime,country,gender,age,handedness,vision,english,browser,count1_Cat,count1_Feat,count1_Prop,count2_Cat,count2_Feat,count2_Prop)

questData_12<-questData

### CLEAN UP DATAFRAME ###
dataAnalysis <- tbl_df(data)
dataAnalysis$Exp<-NA
dataAnalysis[dataAnalysis$Subject < 51,]$Exp <- "E1"
dataAnalysis[dataAnalysis$Subject > 51,]$Exp <- "E2"

dataAnalysis[dataAnalysis$subBlock == 1 | dataAnalysis$subBlock == 3,]$subBlock <- "B1"
dataAnalysis[dataAnalysis$subBlock == 2 | dataAnalysis$subBlock == 4,]$subBlock <- "B2"


dataAnalysis <- dataAnalysis %>%
  filter(trialColor != "quiz",
         Subject != 109) #INCOMPLETE

questData_12 <- questData_12 %>%
  filter(subnum != 109) #INCOMPLETE


dataAnalysis <- as.data.frame(lapply(dataAnalysis, function (x) if (is.factor(x)) factor(x) else x)) 

dataAnalysis$time1<-as.numeric(as.character(dataAnalysis$time1))
dataAnalysis$time2<-as.numeric(as.character(dataAnalysis$time2))

dataAnalysis$ACC<-as.numeric(as.character(dataAnalysis$ACC))
dataAnalysis$RT <- dataAnalysis$time2 - dataAnalysis$time1

dataAnalysis$trialProp<-factor(dataAnalysis$trialProp)
levels(dataAnalysis$trialProp)<-c("biased","biased","unbiased")
dataAnalysis$trialProp <- factor(dataAnalysis$trialProp)

levels(dataAnalysis$countProp) <- c("0% Con", "100% Con")
dataAnalysis$countProp <- factor(dataAnalysis$countProp, levels = c("100% Con","0% Con"))



### SAVE DATAFRAME ###
quizData_12<-quizAnalysis %>%
  filter(Subject%in%unique(dataAnalysis$Subject))%>%
  mutate(RT = as.numeric(as.character(time2))-as.numeric(as.character(time1))) %>%
  select(Exp,Subject,trial,correctResponse,response,ACC,fineACC,RT)

E12<-dataAnalysis %>%
  select(Exp,Subject,trial,correcResponse,response,ACC,fineACC,RT)


### LOAD DATA ###
load("E3.Rda")
load("E3quiz.Rda")
load("E3q.Rda")


### IDENTIFY REPEAT SUBJECTS ###
load("E3ids.Rda")
ids<-gsub("[^[:alnum:][:blank:]+?&/\\-]", "", ids)
E3ids<-ids
load("E12ids.Rda")
ids<-gsub("[^[:alnum:][:blank:]+?&/\\-]", "", ids)
E12ids<-ids
repeatSub<-intersect(E3ids,E12ids)
#repeatSub<-repeatSub[-11]
### END REPEAT SUBJECTS ###

### GET QUESTIONNAIRE DATA ###
questData$expTime<- ((as.numeric(as.character(questData$completeTime)) - as.numeric(as.character(questData$startTime)))/1000)/60
questData$Exp<- "E3"
questData <- questData %>%
  select(Exp,subnum,expTime,country,gender,age,handedness,vision,english,browser,count1_Cat,count1_Feat,count1_Prop,count2_Cat,count2_Feat,count2_Prop)
questData$subnum = questData$subnum + 110
### END QUESTIONNAIRE DATA ####


### ANALYZE QUIZ DATA ###
quizAnalysis <-tbl_df(quizData)
quizAnalysis$Subject<-quizAnalysis$Subject +110
quizAnalysis$response<-as.numeric(as.character(quizAnalysis$response))
quizAnalysis$cResponse<-as.numeric(as.character(quizAnalysis$cResponse))
quizAnalysis$ACC<-as.numeric(as.character(quizAnalysis$ACC))
quizAnalysis$fineACC<-abs(quizAnalysis$response - quizAnalysis$cResponse)

names(quizAnalysis)[names(quizAnalysis)=="cResponse"]  <- "correctResponse"

quiz.Sum<- quizAnalysis %>%
  group_by(Subject) %>%
  filter(!is.na(fineACC)) %>%
  summarise(ACC = mean(ACC), fineACC = mean(fineACC)) 

quiz.Sum
#### END QUIZ DATA ###


### CLEAN UP DATAFRAME ###
dataAnalysis <- tbl_df(data)
dataAnalysis$Exp<-"E3"
dataAnalysis$Subject<-dataAnalysis$Subject + 110

dataAnalysis[dataAnalysis$subBlock == 1 | dataAnalysis$subBlock == 3,]$subBlock <- "B1"
dataAnalysis[dataAnalysis$subBlock == 2 | dataAnalysis$subBlock == 4,]$subBlock <- "B2"


dataAnalysis <- dataAnalysis %>%
  filter(trialGender != "quiz",
         ID%in%repeatSub == FALSE,
         Subject != 139,
         Subject != 157)

dataAnalysis <- as.data.frame(lapply(dataAnalysis, function (x) if (is.factor(x)) factor(x) else x)) 

dataAnalysis$time1<-as.numeric(as.character(dataAnalysis$time1))
dataAnalysis$time2<-as.numeric(as.character(dataAnalysis$time2))

dataAnalysis$ACC<-as.numeric(as.character(dataAnalysis$ACC))
dataAnalysis$RT <- dataAnalysis$time2 - dataAnalysis$time1

dataAnalysis$trialProp<-factor(dataAnalysis$trialProp)
levels(dataAnalysis$trialProp)<-c("biased","biased","unbiased")
dataAnalysis$trialProp <- factor(dataAnalysis$trialProp)

levels(dataAnalysis$countProp) <- c("0% Con", "100% Con")
dataAnalysis$countProp <- factor(dataAnalysis$countProp, levels = c("100% Con","0% Con"))

### SAVE AND BIND DATAFRAMES ###
dataAnalysis$Exp<-"E3"

E3<- dataAnalysis %>%
  select(Exp,Subject,order,block,subBlock,trial,countProp,trialProp,trialCongruency,ACC,RT)

questData <- questData %>%
  filter(subnum%in%unique(E3$Subject))

quizAnalysis$Exp <- "E3"
quizData <- quizAnalysis %>%
  filter(Subject%in%unique(E3$Subject)) %>%
  mutate(RT = as.numeric(as.character(time2))-as.numeric(as.character(time1))) %>%
  select(Exp,Subject,trial,correctResponse,response,ACC,fineACC,RT)



questData<-rbind(questData_12,questData)
dataAnalysis<-rbind(E12,E3)
quizData<-rbind(quizData_12,quizData)


names(quizData)[names(quizData)=="fineACC"] <- "Err_diff"
counting_data<-quizData

dataAnalysis[is.na(dataAnalysis$ACC),]$ACC<-0
dataAnalysis$trial<-as.numeric(as.character(dataAnalysis$trial))

demographics<-questData

raw_data<-dataAnalysis
names(raw_data)[names(raw_data)=="block"]  <- "phase"
levels(raw_data$phase)<-c("1","2")
names(raw_data)[names(raw_data)=="subBlock"] <- "block"
levels(raw_data$block)<-c("1","2")
levels(raw_data$order)<-c("0-100","100-0")

save(demographics,file="demographics.Rda")
save(raw_data, file="raw_data.Rda")
save(counting_data, file="counting_data.Rda")