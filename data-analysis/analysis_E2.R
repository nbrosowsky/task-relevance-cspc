
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



load("data-analysis/raw_data_E2.Rda")
load("data-analysis/demographics_E2.Rda")



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
RT.Analysis <- RT.DF %>%
  group_by(Subject,PC,Congruency) %>%
  summarise(meanRT = mean(vjoutRT))

RT_ANOVA_E2 <- aov_ez("Subject", "meanRT", RT.Analysis, within = c("PC","Congruency"), anova_table = list(correction = "none", es = "none"))
RT_ANOVA_E2 <- apa_print(RT_ANOVA_E2, es = "pes", correction = "none")$full_result

## Analyze ACC for frequency unbiased items
ACC.analysis <- raw_data_E2 %>%
  mutate(error = (1-ACC)*100) %>%
  filter(Subject%in%low_acc == FALSE)  %>%
  group_by(Subject,PC ,Congruency) %>%
  summarise(error = mean(error))


ACC_ANOVA_E2 <- aov_ez("Subject", "error", ACC.analysis, within = c("PC","Congruency"), anova_table = list(correction = "none", es = "none"))
ACC_ANOVA_E2 <- suppressWarnings(apa_print(ACC_ANOVA_E2, es = "pes", correction = "none")$full_result)

####### GRAPH FLANKER EFFECTS #########
RT.Diff <- raw_data_E2 %>%
  filter(
    RT < 3000,
    RT > 0,
    ACC == TRUE,
    Subject%in%low_acc == FALSE
  ) %>%
  group_by(Subject,Frequency, PC,Congruency)%>%
  summarise(RT = vjoutNR(RT,1)) %>%
  spread(Congruency,RT) %>%
  mutate(Diff = inc - con) %>%
  select(-con:-inc) %>%
  group_by(Frequency, PC) %>%
  summarise(
    N = n_distinct(Subject), 
    Flanker = mean(Diff), 
    sd = sd(Diff), 
    SE = sd/sqrt(N)
  ) 


limits <- aes(ymax = Flanker + SE, ymin = Flanker - SE)
RT.graph <- ggplot(RT.Diff,aes(x=PC, y=Flanker,fill=PC))+
  geom_bar(stat="identity", position=position_dodge(width=1), colour = "black", width = c(1, 1,1)) + 
  scale_fill_manual(values=c("#31a354","#addd8e", "#f7fcb9"))+
  geom_errorbar(limits, width = .2, position=position_dodge(width=0.9))+
  coord_cartesian(ylim = c(0,150))+
  scale_y_continuous(breaks=seq(0, 150, 10), expand = c(0,0))+
  theme(axis.text=element_text(size=7.5),
        axis.title=element_text(size=10,face="bold")) +
  labs(title="")+
  ylab("Congruency Effect (ms)")+
  xlab("Proportion\nCongruent") +
  theme(       legend.position=c(1,1),
               legend.justification = c(1,1),
               legend.direction = "horizontal",
               legend.background = element_rect(colour = "black", fill = "white", size=1),
               legend.box.background = element_rect(colour = "black"),
               legend.margin = margin(t = .1, r = .10, b = .05, l = .1, unit = "cm"),
               legend.text = element_text(size = 8),
               legend.title = element_text(size = 8)
  )+
  guides(fill=guide_legend(title="Proportion Congruent",title.position = "top")) + 
  theme(legend.position = "none")


ACC.Diff <- raw_data_E2 %>%
  mutate(error = (1-ACC)*100) %>%
  filter(
    Subject%in%low_acc == FALSE)  %>%
  group_by(Subject,PC,Congruency) %>%
  summarise(error = mean(error)) %>%
  group_by(Subject,PC,Congruency) %>%
  summarise(
    error = mean(error)
  ) %>%
  spread(Congruency,error) %>%
  mutate(Diff = inc - con) %>%
  select(-con:-inc)%>%
  group_by(PC) %>%
  summarise(
    N = n_distinct(Subject), 
    Flanker = mean(Diff), 
    sd = sd(Diff), 
    SE = sd/sqrt(N)
  ) 

limits <- aes(ymax = Flanker + SE, ymin = Flanker - SE)
ACC.graph <- ggplot(ACC.Diff,aes(x=PC, y=Flanker,fill=PC))+
  geom_bar(stat="identity", position=position_dodge(width=1), colour = "black", width = c(1, 1,1)) + 
  scale_fill_manual(values=c("#31a354","#addd8e", "#f7fcb9"))+
  geom_errorbar(limits, width = .2, position=position_dodge(width=0.9))+
  coord_cartesian(ylim = c(0,10))+
  scale_y_continuous(breaks=seq(0, 10, 1), expand = c(0,0))+
  theme(axis.text=element_text(size=7.5),
        axis.title=element_text(size=10,face="bold")) +
  labs(title="")+
  ylab("Congruency Effect (% Error)")+
  xlab("Proportion\nCongruent") +
  theme(       legend.position=c(1,1),
               legend.justification = c(1,1),
               legend.direction = "horizontal",
               legend.background = element_rect(colour = "black", fill = "white", size=1),
               legend.box.background = element_rect(colour = "black"),
               legend.margin = margin(t = .1, r = .10, b = .05, l = .1, unit = "cm"),
               legend.text = element_text(size = 8),
               legend.title = element_text(size = 8)
  ) +
  guides(fill=guide_legend(title="Proportion Congruent",title.position = "top")) + 
  theme(legend.position = "none")


########## ARRANGE #########
figure3<-plot_grid(RT.graph,NULL,ACC.graph, 
                   nrow = 1, 
                   rel_widths = c(1, 0.05, 1),
                   labels = c("A", "", "B"))

#title <- ggdraw() + draw_label("Experiment 2", fontface='bold')
#figure3<-plot_grid(title, figure3, ncol=1, rel_heights=c(0.1, 1)) # rel_heights values control title margins

 ggsave("figure3.pdf", device = "pdf", dpi = 600,
       width = 3.25, height = 3.75, units = "in") 

#clean up environment
#rm(list=c("ACC.analysis","aov.out","raw_data_E2","RT.Analysis","RT.DF","low_acc","vjoutNR", "RT.Diff"))

 
 ################### Bayesian Analysis #####################
 
 ## Analyze RTs for frequency unbiased items
 RT.Analysis <- RT.DF %>%
   ungroup() %>%
   mutate(Subject = factor(Subject),
          PC = factor(PC)) %>%
   group_by(Subject,PC,Congruency) %>%
   summarise(meanRT = mean(vjoutRT)) %>%
   spread(Congruency, meanRT) %>%
   ungroup()%>%
   mutate(Diff = inc - con)
 
 
 
 RT_BF_E2 = BayesFactor::anovaBF(Diff ~ PC + Subject, data = RT.Analysis,
                           whichRandom="Subject", iterations=10000)
 
