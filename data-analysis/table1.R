library(dplyr)
library(tidyr)
library(afex)
library(ggplot2)
library(cowplot)
#As of 2018/12/07 apa_print() requires development version of Papaja
#devtools::install_github("crsh/papaja")
library(papaja)


folderName <- function(){
  wd<-getwd()
  wd<-strsplit(wd,"/")
  wd<-wd[[1]][length(wd[[1]])]
  
  return(wd)
}


vjoutNR <- function(x,n) {
  xm <- mean(x)
  xsize <- c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 20, 
             25, 30, 35, 50, 80)
  stds <- c(1.3, 1.458, 1.68, 1.841, 1.961, 2.05, 2.12, 2.173, 
            2.22, 2.246, 2.274, 2.31, 2.326, 2.391, 2.41, 2.4305, 
            2.45, 2.48, 2.5)
  stdindex <- length(xsize[xsize <= length(x)])
  removed <- x[x < xm+sd(x)*stds[stdindex]]
  removed <- removed[removed > xm - (sd(x)*stds[stdindex])]
  proportionRemoved <- length(removed)/length(x)
  finaldata<-c(mean(removed),1-proportionRemoved)
  return(finaldata[[n]])
}


## manuscript.Rmd is called from parent
load("data-analysis/raw_data_E1.Rda")




# Find subs with < 75% accuracy
low_acc <- raw_data %>%
  group_by(Subject,Condition,Task_Relevant_Context,Frequency,Congruency) %>%
  summarise(meanAccuracy = mean(ACC)) %>%
  filter(meanAccuracy < .75) %>%
  .$Subject

########## Create table


## summarise by subject
RT.DF <- raw_data %>%
  filter(
    RT < 3000,
    RT > 0,
    ACC == TRUE,
    Subject%in%low_acc == FALSE
  ) %>%
  group_by(Condition,Subject,Frequency,Order,Phase,Block,Frequency,Task_Relevant_Context,Congruency)%>%
  summarise(vjoutRT = vjoutNR(RT,1)) %>%
  group_by(Condition,Subject,Frequency,Task_Relevant_Context,Congruency) %>%
  summarise(meanRT = mean(vjoutRT)) %>%
  group_by(Condition,Frequency,Task_Relevant_Context,Congruency) %>%
  summarise(N = n_distinct(Subject),
            RT = mean(meanRT),
            sd = sd(meanRT),
            se = std/sqrt(N))

RT.DF$PC <- NA
RT.DF[RT.DF$Frequency == "unbiased",]$PC <- "50%"
RT.DF[RT.DF$Frequency == "biased" & RT.DF$Congruency == "inc",]$PC <- "0%"
RT.DF[RT.DF$Frequency == "biased" & RT.DF$Congruency == "con",]$PC <- "100%"
names(RT.DF)[names(RT.DF)=="RT"]<-"oldRT"
RT.DF$PC<-as.factor(RT.DF$PC)


RT.table <- RT.DF %>%
  mutate(RT = paste(as.character(round(oldRT))," (", as.character(round(se)), ")", sep="")) %>%
  ungroup() %>%
  select(Condition,PC,Task_Relevant_Context,Congruency,RT)


## summarise by subject
ACC.DF <- raw_data %>%
  filter(
    Subject%in%low_acc == FALSE
  ) %>%
  group_by(Condition,Subject,Frequency,Order,Phase,Block,Frequency,Task_Relevant_Context,Congruency)%>%
  summarise(ER = (1-mean(ACC))*100)  %>%
  group_by(Condition,Subject,Frequency,Task_Relevant_Context,Congruency) %>%
  summarise(meanER = mean(ER)) %>%
  group_by(Condition,Frequency,Task_Relevant_Context,Congruency) %>%
  summarise(N = n_distinct(Subject),
            ER = mean(meanER),
            std = sd(meanER),
            se = std/sqrt(N))

ACC.DF$PC <- NA
ACC.DF[ACC.DF$Frequency == "unbiased",]$PC <- "50%"
ACC.DF[ACC.DF$Frequency == "biased" & ACC.DF$Congruency == "inc",]$PC <- "0%"
ACC.DF[ACC.DF$Frequency == "biased" & ACC.DF$Congruency == "con",]$PC <- "100%"
names(ACC.DF)[names(ACC.DF)=="ER"]<-"oldER"
ACC.DF$PC<-as.factor(ACC.DF$PC)

ACC.table <- ACC.DF %>%
  mutate(ER = paste(as.character(round(oldER,digits=2))," (", as.character(round(se, digits=2)), ")", sep="")) %>%
  ungroup() %>%
  select(Condition,PC,Task_Relevant_Context,Congruency,ER)

table<-merge(RT.table,ACC.table)
names(table)[names(table)=="Task_Relevant_Context"]<-"Task Relevant Context"
levels(table$`Task Relevant Context`)<-c("Task Relevant Context: 100% PC", "Task Relevant Context: 0% PC")
levels(table$Condition)<-c("Object","Social","Social (NR)")
table$PC <- factor(table$PC, levels = c("0%", "50%", "100%"))



## need to reshape because kable cannot do nested tables automatically
t100<-(table[table$`Task Relevant Context` == "Task Relevant Context: 100% PC",])
t100<-t100[,-3]
t100<-rbind(t100,
            c("Object","0%","con","-","-","-","-"),
            c("Social","0%","con","-","-","-","-"),
            c("Social (NR)","0%","con","-","-","-","-"),
            c("Object","100%","inc","-","-","-","-"),
            c("Social","100%","inc","-","-","-","-"),
            c("Social (NR)","100%","inc","-","-","-","-")
)

t100<-cbind(
  t100[t100$Congruency == "con",] %>%
    arrange(Condition,PC), 
  t100[t100$Congruency == "inc",] %>%
    arrange(Condition,PC))

t100<-t100[c(1,2,4,5,9,10)]
colnames(t100)<-c("Condition","PC","RT","ER","RT","ER")


t0<-cbind(table[table$`Task Relevant Context` == "Task Relevant Context: 0% PC",])
t0<-t0[,-3]
t0<-rbind(t0,
          c("Object","0%","con","-","-","-","-"),
          c("Social","0%","con","-","-","-","-"),
          c("Social (NR)","0%","con","-","-","-","-"),
          c("Object","100%","inc","-","-","-","-"),
          c("Social","100%","inc","-","-","-","-"),
          c("Social (NR)","100%","inc","-","-","-","-")
)
t0<-cbind(
  t0[t0$Congruency == "con",] %>%
    arrange(Condition,PC), 
  t0[t0$Congruency == "inc",] %>%
    arrange(Condition,PC))
t0<-t0[,c(1,2,4,5,9,10)]
colnames(t0)<-c("Condition","PC","RT","ER","RT","ER")


table<-rbind(t100,t0)
colnames(table)<-c("Condition","PC","RT","ER","RT","ER")

table$TRC<-as.factor(c(rep("100% PC", 9), rep("0% PC", 9)))
table<-table[c(7,1:6)]
colnames(table)<-c("Task-Relevant Context","Condition","PC","RT","ER","RT","ER")

forTable<-as.matrix(table[,4:7])

TR_Table<-paste(
    "\\begin{table}[htbp]",
    "\\caption{Reaction times and error rates from Experiment 1.}",
    "\\label{TR_table}",
    "\\centering",
    "\\begin{tabular}{lllcccc}",
    "\\toprule",
    " & & & \\multicolumn{2}{c}{Congruent} & \\multicolumn{2}{c}{Incongruent} \\\\", 
    "\\cmidrule(rl){4-5}",
    "\\cmidrule(rl){6-7}",
    "\\multicolumn{1}{c}{Task-Relevant Context} & \\multicolumn{1}{c}{Condition} & \\multicolumn{1}{c}{PC} & \\multicolumn{1}{c}{RT} & \\multicolumn{1}{c}{ER} & \\multicolumn{1}{c}{RT} & \\multicolumn{1}{c}{ER}  \\\\",
    "\\midrule", 
    paste0("\\multirow{9}{*}{100\\% PC} & \\multirow{3}{*}{Object} & \\multicolumn{1}{l}{0\\%} & \\multicolumn{1}{l}{", paste(forTable[1,],collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
    paste0("& & \\multicolumn{1}{l}{50\\%} & \\multicolumn{1}{l}{", paste(forTable[2,],collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
    paste0("& & \\multicolumn{1}{l}{100\\%} & \\multicolumn{1}{l}{", paste(forTable[3,],collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
    "\\cmidrule(rl){2-7}",
    paste0("& \\multirow{3}{*}{Social} & \\multicolumn{1}{l}{0\\%} & \\multicolumn{1}{c}{", paste(forTable[4,],collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
    paste0("& & \\multicolumn{1}{l}{50\\%} & \\multicolumn{1}{l}{", paste(forTable[5,],collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
    paste0("& & \\multicolumn{1}{l}{100\\%} & \\multicolumn{1}{l}{", paste(forTable[6,],collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
    "\\cmidrule(rl){2-7}",
    paste0("& \\multirow{3}{*}{Social (NR)} & \\multicolumn{1}{l}{0\\%} & \\multicolumn{1}{l}{", paste(forTable[7,],collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
    paste0("& & \\multicolumn{1}{l}{50\\%} & \\multicolumn{1}{l}{", paste(forTable[8,],collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
    paste0("& & \\multicolumn{1}{l}{100\\%} & \\multicolumn{1}{l}{", paste(forTable[9,],collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
    "\\midrule", 
    
    paste0("\\multirow{9}{*}{0\\% PC} & \\multirow{3}{*}{Object} & \\multicolumn{1}{l}{0\\%} & \\multicolumn{1}{l}{", paste(forTable[10,],collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
    paste0("& & \\multicolumn{1}{l}{50\\%} & \\multicolumn{1}{l}{", paste(forTable[11,],collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
    paste0("& & \\multicolumn{1}{l}{100\\%} & \\multicolumn{1}{l}{", paste(forTable[12,],collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
    "\\cmidrule(rl){2-7}",
    paste0("& \\multirow{3}{*}{Social} & \\multicolumn{1}{l}{0\\%} & \\multicolumn{1}{l}{", paste(forTable[13,],collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
    paste0("& & \\multicolumn{1}{l}{50\\%} & \\multicolumn{1}{l}{", paste(forTable[14,],collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
    paste0("& & \\multicolumn{1}{l}{100\\%} & \\multicolumn{1}{l}{", paste(forTable[15,],collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
    "\\cmidrule(rl){2-7}",
    paste0("& \\multirow{3}{*}{Social (NR)} & \\multicolumn{1}{l}{0\\%} & \\multicolumn{1}{l}{", paste(forTable[16,],collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
    paste0("& & \\multicolumn{1}{l}{50\\%} & \\multicolumn{1}{l}{", paste(forTable[17,],collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
    paste0("& & \\multicolumn{1}{l}{100\\%} & \\multicolumn{1}{l}{", paste(forTable[18,],collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
    
    " & & & & & & \\\\",
    "\\bottomrule",
    "\\multicolumn{7}{l}{\\textit{Note}: RT = Reaction Time (ms);  ER = Error Rates (\\%); PC = Proportion Congruent;} \\\\",
    "\\multicolumn{7}{l}{NR = Non-Repeating; Standard Errors are presented in parantheses.} \\\\",
    "\\end{tabular}%",
    "\\end{table}",
    sep = "\n"
)

print(kableExtra::kable(TR_Table,format="latex"))
    