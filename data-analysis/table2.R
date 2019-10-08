
library(dplyr)
library(tidyr)
library(afex)
library(ggplot2)
library(cowplot)
library(BayesFactor)
#As of 2018/12/07 apa_print() requires development version of Papaja
#devtools::install_github("crsh/papaja")
library(papaja)
library(kableExtra)

# Load functions
source("data-analysis/print_apa_ci.R")
source("data-analysis/print_bf.R")
source("data-analysis/outlier_removal.R")

# Load data
load("data-analysis/raw_data_E2.Rda")
load("data-analysis/demographics_E2.Rda")

theme_set(theme_cowplot())
# Find subs with < 75% accuracy
low_acc <- raw_data_E2 %>%
  group_by(Subject,Condition,PC,Congruency) %>%
  summarise(meanAccuracy = mean(ACC)) %>%
  filter(meanAccuracy < .75) %>%
  .$Subject

N_subjects_removed_E2 <-length(unique(low_acc))

## summarise by subject
RT.DF <- raw_data_E2 %>%
  filter(
    RT < 3000,
    RT > 0,
    ACC == TRUE,
    Subject%in%low_acc == FALSE
  ) %>%
  group_by(Subject,PC,Congruency)%>%
  summarise(vjoutRT = vjoutNR(RT,1))

## Find percentage of trials removed 
percent_removed_E2 <- raw_data_E2 %>%
  filter(
    RT < 3000,
    RT > 0,
    ACC == TRUE,
    Subject%in%low_acc == FALSE
  ) %>%
  group_by(Subject,PC,Congruency)%>%
  summarise(percent_removed = vjoutNR(RT,2)) %>%
  ungroup() %>%
  summarise(avg_removed = mean(percent_removed)) %>%
  .$avg_removed

## Analyze RTs 
RT.DF <- RT.DF %>%
  ungroup() %>%
  mutate(Subject = factor(Subject),
         PC = factor(PC)) %>%
  group_by(Subject,PC,Congruency) %>%
  summarise(meanRT = mean(vjoutRT)) %>%
  group_by(PC,Congruency) %>%
  summarise(N = n_distinct(Subject),
            RT = mean(meanRT),
            std = sd(meanRT),
            se = std/sqrt(N))

RT.table <- RT.DF %>%
  mutate(RT = paste(as.character(round(RT))," (", as.character(round(se)), ")", sep="")) %>%
  ungroup() %>%
  select(PC,Congruency,RT)



## Analyze ACC for frequency unbiased items
ACC.DF <- raw_data_E2 %>%
  mutate(error = (1-ACC)*100,
         Subject = factor(Subject),
         PC = factor(PC)) %>%
  filter(
    Subject%in%low_acc == FALSE)  %>%
  group_by(Subject,PC,Congruency) %>%
  summarise(error = mean(error)) %>%
  group_by(Subject,PC,Congruency) %>%
  summarise(
    meanER = mean(error)
  )  %>%
  group_by(PC,Congruency) %>%
  summarise(N = n_distinct(Subject),
            ER = mean(meanER),
            std = sd(meanER),
            se = std/sqrt(N))


ACC.table <- ACC.DF %>%
  mutate(ER = paste(as.character(round(ER,digits=2))," (", as.character(round(se, digits=2)), ")", sep="")) %>%
  ungroup() %>%
  select(PC,Congruency,ER)


table<-merge(RT.table,ACC.table)

table <- cbind(
  table[table$Congruency == "con",] %>%
    arrange(PC), 
  table[table$Congruency == "inc",] %>%
    arrange(PC))

table<-table[,c(1,3,4,7,8)]


forTable<-as.matrix(table[,2:5])

TR_Table_2 <-paste(
  "\\begin{table}[htbp]",
  "\\caption{Reaction times and error rates from Experiment 2.}",
  "\\label{TR_table_2}",
  "\\centering",
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  "& \\multicolumn{2}{c}{Congruent} & \\multicolumn{2}{c}{Incongruent} \\\\", 
  "\\cmidrule(rl){2-3}",
  "\\cmidrule(rl){4-5}",
  "\\multicolumn{1}{c}{PC} & \\multicolumn{1}{c}{RT} & \\multicolumn{1}{c}{ER} & \\multicolumn{1}{c}{RT} & \\multicolumn{1}{c}{ER}  \\\\",
  "\\midrule", 
  paste0("\\multicolumn{1}{l}{25\\%} & \\multicolumn{1}{l}{", paste(forTable[1,],collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
  paste0("\\multicolumn{1}{l}{50\\%} & \\multicolumn{1}{l}{", paste(forTable[2,],collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
  paste0("\\multicolumn{1}{l}{100\\%} & \\multicolumn{1}{l}{", paste(forTable[3,],collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
  "\\bottomrule",
  "\\multicolumn{5}{l}{\\textit{Note}: RT = Reaction Time (ms);  ER = Error Rates (\\%);} \\\\",
  "\\multicolumn{5}{l}{PC = Proportion Congruent; Standard Errors} \\\\",
  "\\multicolumn{5}{l}{are presented in parantheses.} \\\\",
  "\\end{tabular}%",
  "\\end{table}",
  sep = "\n"
)
