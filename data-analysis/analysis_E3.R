
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



load("data-analysis/raw_data_E3.Rda")
# load("data-analysis/demographics_E3.Rda")
# demographics_E3$Subject <- demographics_E3$subnum
# 
# raw_data_E3 <- inner_join(raw_data_E3, demographics_E3, by = "Subject")
# raw_data_E3 <- raw_data_E3 %>%
#   mutate(task_feedback = as.numeric(as.character(task_feedback))) %>%
#   filter(!is.na(task_feedback),
#                 task_feedback > 3)

# Find subs with < 75% accuracy
low_acc <- raw_data_E3 %>%
  group_by(Subject,Condition,Task_Relevant_Context,Frequency,Congruency) %>%
  summarise(meanAccuracy = mean(ACC)) %>%
  filter(meanAccuracy < .75) %>%
  .$Subject

N_subjects_removed_E3 <-length(unique(low_acc))

## summarise by subject
RT.DF <- raw_data_E3 %>%
  filter(
    RT < 3000,
    RT > 0,
    ACC == TRUE,
    Subject%in%low_acc == FALSE
  ) %>%
  group_by(Condition,Subject,Order,Phase,Block,Frequency,Task_Relevant_Context,Congruency)%>%
  summarise(vjoutRT = vjoutNR(RT,1))

## Find percentage of trials removed 
percent_removed_E3 <- raw_data_E3 %>%
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
  summarise(meanRT = mean(vjoutRT))

RT_ANOVA_E3 <- aov_car(meanRT ~ Task_Relevant_Context*Congruency + Error(Subject/Task_Relevant_Context*Congruency), data = RT.Analysis)
RT_ANOVA_E3 <- apa_print(RT_ANOVA_E3, es = "pes")$full_result

## Analyze ACC for frequency unbiased items
ACC.analysis <- raw_data_E3 %>%
  mutate(error = (1-ACC)*100) %>%
  filter(
    Frequency == "unbiased",
    Subject%in%low_acc == FALSE)  %>%
  group_by(Subject,Condition,Task_Relevant_Context,Congruency) %>%
  summarise(error = mean(error))

ACC_ANOVA_E3 <- aov_car(error ~ Task_Relevant_Context*Congruency + Error(Subject/Task_Relevant_Context*Congruency), data = ACC.analysis)
ACC_ANOVA_E3 <- apa_print(ACC_ANOVA_E3, es = "pes")$full_result

####### GRAPH FLANKER EFFECTS #########
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
#levels(RT.Diff$Condition) <- c("Object", "Social", "Social (NR)")

limits <- aes(ymax = Flanker + SE, ymin = Flanker - SE)
RT.graph <- ggplot(RT.Diff,aes(x=Task_Relevant_Context, y=Flanker,fill=Task_Relevant_Context))+
  geom_bar(stat="identity", position=position_dodge(width=0.9), colour = "black", width = 1) + 
  scale_fill_manual(values=c("#0570b0","#bdc9e1"))+
  geom_errorbar(limits, width = .2, position=position_dodge(width=0.9))+
  coord_cartesian(ylim = c(0,150))+
  scale_y_continuous(breaks=seq(0, 150, 10), expand = c(0,0))+
  theme(axis.text=element_text(size=7.5),
        axis.title=element_text(size=10,face="bold")) +
  labs(title="")+
  xlab("Task-Relevant Context") +
  ylab("Congruency Effect (ms)")+
  theme(       legend.position=c(1,1),
               legend.justification = c(1,1),
               legend.direction = "vertical",
               legend.background = element_rect(colour = "black", fill = "white", size=1),
               legend.box.background = element_rect(colour = "black"),
               legend.margin = margin(t = .1, r = .10, b = .05, l = .1, unit = "cm"),
               legend.text = element_text(size = 8),
               legend.title = element_text(size = 8),
               axis.title.x=element_blank(),
               axis.text.x=element_blank(),
               axis.ticks.x=element_blank()
  ) +
  guides(fill=guide_legend(title="Task-Relevant \nContext",title.position = "top")) +
  theme(legend.position = "none")


RT.Diff<-RT.DF %>%
  filter(Frequency == "unbiased") %>%
  group_by(Subject,Task_Relevant_Context,Congruency) %>%
  summarise(
    RT = mean(vjoutRT)
  ) %>%
  spread(Congruency,RT) %>%
  mutate(Diff = inc - con) %>%
  select(-con:-inc)%>%
  group_by(Subject,Task_Relevant_Context) %>% 
  spread(Task_Relevant_Context,Diff) 

ACC.Diff <- raw_data_E3 %>%
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


levels(ACC.Diff$Task_Relevant_Context) <- c("100% PC", "0% PC")
#levels(ACC.Diff$Condition) <- c("Object", "Social", "Social (NR)")

limits <- aes(ymax = Flanker + SE, ymin = Flanker - SE)
ACC.graph <- ggplot(ACC.Diff,aes(x=Task_Relevant_Context, y=Flanker,fill=Task_Relevant_Context))+
  geom_bar(stat="identity", position=position_dodge(width=.3), colour = "black", width = 1) + 
  scale_fill_manual(values=c("#0570b0","#bdc9e1"))+
  geom_errorbar(limits, width = .2, position=position_dodge(width=0.9))+
  coord_cartesian(ylim = c(0,10))+
  scale_y_continuous(breaks=seq(0, 10, 1), expand = c(0,0))+
  theme(axis.text=element_text(size=7.5),
        axis.title=element_text(size=10,face="bold")) +
  labs(title="")+
  xlab("Task-Relevant Context") +
  ylab("Congruency Effect (% Error)")+
  theme(       legend.position=c(1,1),
               legend.justification = c(1,1),
               legend.direction = "vertical",
               legend.background = element_rect(colour = "black", fill = "white", size=1),
               #legend.box.background = element_rect(colour = "black"),
               legend.margin = margin(t = .1, r = .10, b = .05, l = .1, unit = "cm"),
               legend.text = element_text(size = 8),
               legend.title = element_text(size = 8),
               axis.title.x=element_blank(),
               axis.text.x=element_blank(),
               axis.ticks.x=element_blank()
  ) +
  guides(fill=guide_legend(title="Task-Relevant \nContext",title.position = "top")) 


########## ARRANGE #########
figure4<-plot_grid(RT.graph,NULL,ACC.graph, 
                   nrow = 1, 
                   rel_widths = c(1, 0.05, 1),
                   labels = c("A", "", "B"))

#title <- ggdraw() + draw_label("Experiment 3", fontface='bold')
#figure4<-plot_grid(title, figure4, ncol=1, rel_heights=c(0.1, 1)) # rel_heights values control title margins

ggsave("figure4.pdf", device = "pdf", dpi = 600,
      width = 3.25, height = 3.75, units = "in") 

#clean up environment
#rm(list=c("ACC.analysis","aov.out","raw_data_E3","RT.Analysis","RT.DF","low_acc","vjoutNR", "RT.Diff"))


################### Bayesian Analysis #####################

## Analyze RTs for frequency unbiased items
RT.Analysis <- RT.DF %>%
  filter(
    Frequency == "unbiased"
  ) %>%
  ungroup() %>%
  mutate(Subject = factor(Subject),
         Task_Relevant_Context = factor(Task_Relevant_Context)) %>%
  group_by(Subject,Task_Relevant_Context,Congruency) %>%
  summarise(meanRT = mean(vjoutRT)) %>%
  spread(Congruency, meanRT) %>%
  mutate(Diff = inc - con) 



RT_BF_E3 = BayesFactor::anovaBF(Diff ~ Task_Relevant_Context + Subject, data = RT.Analysis,
                          whichRandom="Subject",whichModel ="withmain", iterations=10000)
