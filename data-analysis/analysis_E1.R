
library(dplyr)
library(tidyr)
library(afex)
library(ggplot2)
library(cowplot)
library(BayesFactor)
#As of 2018/12/07 apa_print() requires development version of Papaja
#devtools::install_github("crsh/papaja")
library(papaja)
library(apaTables)

set.seed(1983)

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


print_eta_CI <- function(Fval, conf = .90, df1, df2){
  limits <- apaTables::get.ci.partial.eta.squared(F.value=Fval, df1=df1, df2=df2, conf.level=conf)
  return(paste0(", 90\\% CI $[",round(limits$LL, 2),"$, $",round(limits$UL, 2),"]$"))
}

print_apa_ci <- function(aov_table){
  pap <- apa_print(aov_table, es = "pes")$full_result
  
  for(i in 1:length(pap)){
    pap[i] <- paste0(pap[i], print_eta_CI(Fval = aov_table$anova_table$`F`[i], df1 = aov_table$anova_table$`num Df`[i], df2 = aov_table$anova_table$`den Df`[i]))
    pap[i] <- gsub("p = .000", "p < .001", pap[i])
    pap[i] <- gsub(") = 0.00", ") < 0.01", pap[i])
  }
  return(pap)
}

print_bf <- function(bf){
  result <- list()
  for (i in 1:length(bf)){
    if(extractBF(bf[i])$bf < 1){
      result[i] <- paste0("$\\mathrm{BF}_{\\textrm{01}} = ", round(extractBF(1/bf[i])$bf, digits = 2),"$ $[\\pm ", round(extractBF(bf[i])$error*100,digits=2), "\\%]$")
    } else {
      result[i] <- paste0("$\\mathrm{BF}_{\\textrm{10}} = ", round(extractBF(bf[i])$bf, digits = 2),"$ $[\\pm ", round(extractBF(bf[i])$error*100,digits=2), "\\%]$")
      
    }
  }
  
  return(result)
}

################## ANALYSIS #########################
load("data-analysis/raw_data_E1.Rda")


# Find subs with < 75% accuracy
low_acc <- raw_data %>%
  group_by(Subject,Condition,Task_Relevant_Context,Frequency,Congruency) %>%
  summarise(meanAccuracy = mean(ACC)) %>%
  filter(meanAccuracy < .75) %>%
  .$Subject

N_subjects_removed<-length(unique(low_acc))

## summarise by subject
RT.DF <- raw_data %>%
  filter(
    RT < 3000,
    RT > 0,
    ACC == TRUE,
    Subject%in%low_acc == FALSE
  ) %>%
  group_by(Condition,Subject,Order,Phase,Block,Frequency,Task_Relevant_Context,Congruency)%>%
  summarise(vjoutRT = vjoutNR(RT,1))

## Find percentage of trials removed 
percent_removed <- raw_data %>%
  filter(
    RT < 3000,
    RT > 0,
    ACC == TRUE,
    Subject%in%low_acc == FALSE
  ) %>%
  group_by(Condition,Subject,Order,Phase,Frequency,Task_Relevant_Context,Congruency)%>%
  summarise(percent_removed = vjoutNR(RT,2)) %>%
  ungroup() %>%
  summarise(avg_removed = mean(percent_removed)) %>%
  .$avg_removed

## Analyze RTs for frequency unbiased items
RT.Analysis <- RT.DF %>%
  filter(
    Frequency == "unbiased"
  ) %>%
  group_by(Condition,Subject,Task_Relevant_Context,Congruency) %>%
  summarise(meanRT = mean(vjoutRT))%>%
  spread(Congruency,meanRT) %>%
  mutate(CE = inc - con) %>%
  select(-con:-inc)

aov_table <- aov_car(CE ~ Condition*Task_Relevant_Context + Error(Subject/Task_Relevant_Context), data = RT.Analysis)
RT_ANOVA <- print_apa_ci(aov_table)

RT_BF = BayesFactor::anovaBF(CE ~ Condition*Task_Relevant_Context + Subject, data = RT.Analysis,
                             whichRandom="Subject", iterations=10000)

pRT_BF = print_bf(RT_BF)
BF_contrast = paste0("$", round(extractBF(RT_BF[2]/RT_BF[4])$bf, digits = 2), 
                           "$ $[\\pm ", 
                           round(extractBF(RT_BF[2]/RT_BF[4])$error*100, digits = 2),
                           "\\%]$"
)

## Analyze ACC for frequency unbiased items
ACC.analysis <- raw_data %>%
  mutate(error = (1-ACC)*100) %>%
  filter(
    Frequency == "unbiased",
    Subject%in%low_acc == FALSE)  %>%
  group_by(Subject,Condition,Task_Relevant_Context,Congruency) %>%
  summarise(error = mean(error)) %>%
  spread(Congruency,error) %>%
  mutate(CE = inc - con) %>%
  select(-con:-inc)


ACC_aov_table <- aov_car(CE ~ Condition*Task_Relevant_Context + Error(Subject/Task_Relevant_Context), data = ACC.analysis)
ACC_ANOVA <- print_apa_ci(ACC_aov_table)

ACC_BF = BayesFactor::anovaBF(CE ~ Condition*Task_Relevant_Context + Subject, data = ACC.analysis,
                              whichRandom="Subject", iterations=10000)


pACC_BF = print_bf(ACC_BF)
BF_ACC_contrast = paste0("$", round(extractBF(ACC_BF[2]/ACC_BF[4])$bf, digits = 2), 
                     "$ $[\\pm ", 
                     round(extractBF(ACC_BF[2]/ACC_BF[4])$error*100, digits = 2),
                     "\\%]$"
)

##################### GRAPHS ######################
RT.Diff<-RT.DF %>%
  filter(Frequency == "unbiased") %>%
  group_by(Condition,Subject,Task_Relevant_Context,Congruency) %>%
  summarise(
    RT = mean(vjoutRT)
  ) %>%
  spread(Congruency,RT) %>%
  mutate(Diff = inc - con) %>%
  select(-con:-inc)%>%
  group_by(Condition,Task_Relevant_Context) %>%
  summarise(
    N = n_distinct(Subject), 
    Flanker = mean(Diff), 
    sd = sd(Diff), 
    SE = sd/sqrt(N)
  ) 

#names(RT.Diff)[names(RT.Diff) == "Task_Relevant_Context"] <- 'Task Relevant\nContext'
levels(RT.Diff$Task_Relevant_Context) <- c("100% PC", "0% PC")
levels(RT.Diff$Condition) <- c("Object", "Social", "Social (NR)")


limits <- aes(ymax = Flanker + SE, ymin = Flanker - SE)
RT.graph <- ggplot(RT.Diff,aes(x=Condition, y=Flanker,fill=Task_Relevant_Context))+
  geom_bar(stat="identity", position=position_dodge(width=0.9), colour = "black") + 
  scale_fill_manual(values=c("#0570b0","#bdc9e1"))+
  #scale_fill_manual(values=c("#CA3542", "#276478"))+
  geom_errorbar(limits, width = .2, position=position_dodge(width=0.9))+
  coord_cartesian(ylim = c(0,150))+
  scale_y_continuous(breaks=seq(0, 150, 10), expand = c(0,0))+
  theme(axis.text=element_text(size=7.5),
        axis.title=element_text(size=10,face="bold")) +
  labs(title="")+
  ylab("Congruency Effect (ms)")+
  theme(       legend.position=c(1,1),
               legend.justification = c(1,1),
               legend.direction = "horizontal",
               #legend.background = element_rect(colour = "black", fill = "white", size=1),
               #legend.box.background = element_rect(colour = "black"),
               legend.margin = margin(t = .1, r = .10, b = .05, l = .1, unit = "cm"),
               legend.text = element_text(size = 8),
               legend.title = element_text(size = 8)
  ) +
  guides(fill=guide_legend(title="Task-Relevant Context",title.position = "top")) 



ACC.Diff <- raw_data %>%
  mutate(error = (1-ACC)*100) %>%
  filter(
    Frequency == "unbiased",
    Subject%in%low_acc == FALSE)  %>%
  group_by(Subject,Condition,Task_Relevant_Context,Congruency) %>%
  summarise(error = mean(error)) %>%
  spread(Congruency,error) %>%
  mutate(Diff = inc - con) %>%
  select(-con:-inc)%>%
  group_by(Condition,Task_Relevant_Context) %>%
  summarise(
    N = n_distinct(Subject), 
    Flanker = mean(Diff), 
    sd = sd(Diff), 
    SE = sd/sqrt(N)
  ) 



levels(ACC.Diff$Task_Relevant_Context) <- c("100% PC", "0% PC")
levels(ACC.Diff$Condition) <- c("Object", "Social", "Social (NR)")

ACC.Diff <- raw_data %>%
  mutate(error = (1-ACC)*100) %>%
  filter(
    Frequency == "unbiased",
    Subject%in%low_acc == FALSE)  %>%
  group_by(Subject,Condition,Task_Relevant_Context,Congruency) %>%
  summarise(error = mean(error)) %>%
  group_by(Condition,Subject,Task_Relevant_Context,Congruency) %>%
  summarise(
    error = mean(error)
  ) %>%
  spread(Congruency,error) %>%
  mutate(Diff = inc - con) %>%
  select(-con:-inc)%>%
  group_by(Condition,Task_Relevant_Context) %>%
  summarise(
    N = n_distinct(Subject), 
    Flanker = mean(Diff), 
    sd = sd(Diff), 
    SE = sd/sqrt(N)
  ) 


limits <- aes(ymax = Flanker + SE, ymin = Flanker - SE)
ACC.graph <- ggplot(ACC.Diff,aes(x=Condition, y=Flanker,fill=Task_Relevant_Context))+
  geom_bar(stat="identity", position=position_dodge(width=0.9), colour = "black") + 
  scale_fill_manual(values=c("#0570b0","#bdc9e1"))+
  geom_errorbar(limits, width = .2, position=position_dodge(width=0.9))+
  coord_cartesian(ylim = c(0,10))+
  scale_y_continuous(breaks=seq(0, 10, 1), expand = c(0,0))+
  theme(axis.text=element_text(size=7.5),
        axis.title=element_text(size=10,face="bold")) +
  labs(title="")+
  ylab("Congruency Effect (% Error)")+
  theme(       legend.position=c(1,1),
               legend.justification = c(1,1),
               legend.direction = "horizontal",
               #legend.background = element_rect(colour = "white", fill = "white", size=1),
               #legend.box.background = element_rect(colour = "black"),
               legend.margin = margin(t = .1, r = .10, b = .05, l = .1, unit = "cm"),
               legend.text = element_text(size = 8),
               legend.title = element_text(size = 8)
  ) +
  guides(fill=guide_legend(title="Task-Relevant Context",title.position = "top"))

#ACC.graph

########## ARRANGE #########
figure2<-plot_grid(RT.graph,NULL,ACC.graph, 
                   nrow = 1, 
                   rel_widths = c(1, 0.05, 1),
                   labels = c("A", "", "B"))

#title <- ggdraw() + draw_label("Experiment 1", fontface='bold')
#figure2<-plot_grid(title, figure2, ncol=1, rel_heights=c(0.1, 1)) # rel_heights values control title margins

figure2

ggsave("figure2.pdf", device = "pdf", dpi = 600,
      width = 6.875, height = 4, units = "in") 

#clean up environment
#rm(list=c("ACC.analysis","aov.out","raw_data","RT.Analysis","RT.DF","low_acc","vjoutNR", "RT.Diff"))
