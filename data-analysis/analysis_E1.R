
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
  summarise(meanRT = mean(vjoutRT))

RT_ANOVA <- aov_car(meanRT ~ Condition*Task_Relevant_Context*Congruency + Error(Subject/Task_Relevant_Context*Congruency), data = RT.Analysis)
RT_ANOVA <- apa_print(RT_ANOVA, es = "pes")$full_result

## Analyze ACC for frequency unbiased items
ACC.analysis <- raw_data %>%
  mutate(error = (1-ACC)*100) %>%
  filter(
    Frequency == "unbiased",
    Subject%in%low_acc == FALSE)  %>%
  group_by(Subject,Condition,Task_Relevant_Context,Congruency) %>%
  summarise(error = mean(error))

ACC_ANOVA <- aov_car(error ~ Condition*Task_Relevant_Context*Congruency + Error(Subject/Task_Relevant_Context*Congruency), data = ACC.analysis)
ACC_ANOVA <- apa_print(ACC_ANOVA, es = "pes")$full_result

####### GRAPH FLANKER EFFECTS #########
RT.sum<-RT.DF %>%
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
    se = sd/sqrt(N)
  )

WRS2::bwtrim(Diff ~ Condition*Task_Relevant_Context, id = Subject, data = RT.sum, tr = .2)

RT.Diff<-RT.DF %>%
  filter(Frequency == "unbiased") %>%
  group_by(Condition, Subject,Task_Relevant_Context,Congruency) %>%
  summarise(
    RT = mean(vjoutRT)
  ) %>%
  spread(Congruency,RT) %>%
  mutate(Flanker = inc - con) %>%
  select(-con:-inc)



#names(RT.Diff)[names(RT.Diff) == "Task_Relevant_Context"] <- 'Task Relevant\nContext'
levels(RT.Diff$Task_Relevant_Context) <- c("100% PC", "0% PC")
levels(RT.Diff$Condition) <- c("Object", "Social", "Social (NR)")

pp <- ggpubr::ggboxplot(RT.Diff, x = "Condition", y = "Flanker",
          #color = "Task_Relevant_Context", palette =c("#00AFBB", "#FC4E07"),
          color = "Task_Relevant_Context", palette =c("#CA3542", "#276478"),
          notch = FALSE,
          #width = .25,
          add = c("jitter"), shape = "Task_Relevant_Context",
          size = 1.2)

pp    <- pp + scale_y_continuous(limits = c(-125, 375), breaks = seq(from= -100, to=350, by=50 ), expand = c(0,0)) + geom_hline(yintercept = 0, linetype="dashed")

#idx <- which(sapply(pp$layers, function(l) "PositionJitter" %in% class(l$position)))
idx <- which(sapply(pp$layers, function(l) "PositionJitterdodge" %in% class(l$position)))

pp$layers[[1]]$geom_params$outlier.alpha = 0
pp$layers[[idx]]$aes_params$alpha <- 0.2
pp$layers[[idx]]$aes_params$size <- 3
pp$layers[[idx]]$position$jitter.width = .15
pp <- ggpar(p = pp, legend.title = "Task Relevant Context", legend = "bottom")
pp
##########

ggplot(RT.Diff[RT.Diff$Condition == "Object",], aes(x = Condition, y = Flanker, fill = Task_Relevant_Context)) +
  geom_flat_violin(aes(fill = Task_Relevant_Context),position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(Condition)-.15, y = Flanker, colour = Task_Relevant_Context),position = position_jitter(width = .05), size = 1, shape = 20)+
  geom_boxplot(aes(x = Condition, y = Flanker, fill = Task_Relevant_Context),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  scale_colour_manual(values = c("#CA3542", "#276478")) +
  scale_fill_manual(values = c("#CA3542", "#276478")) +
  #  geom_line(data = ACC.sum, aes(x = as.numeric(Condition)+.1, y = Flanker, group = Task_Relevant_Context, colour = Task_Relevant_Context), linetype = 3)+
  geom_point(data = RT.sum[RT.sum$Condition == "Object",], aes(x = as.numeric(Condition)+.1, y = Flanker, group = Task_Relevant_Context, colour = Task_Relevant_Context), shape = 18, size = 3) +
  geom_errorbar(data = RT.sum[RT.sum$Condition == "Object",], aes(x = as.numeric(Condition)+.1, y = Flanker, group = Task_Relevant_Context, colour = Task_Relevant_Context, ymin = Flanker-se, ymax = Flanker+se), width = .08, size = 1)+
#  scale_colour_brewer(palette = "Dark2")+
#  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Figure 10: Repeated Measures Factorial Rainclouds") +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip()



pp <- ggpubr::ggboxplot(RT.Diff, x = "Task_Relevant_Context", y = "Flanker",
                        #color = "Task_Relevant_Context", palette =c("#00AFBB", "#FC4E07"),
                        color = "Task_Relevant_Context", palette =c("#CA3542", "#276478"),
                        notch = FALSE,
                        #width = .25,
                        add = c("jitter"), shape = "Task_Relevant_Context",
                        size = 1.2)

pp    <- pp + scale_y_continuous(limits = c(-125, 375), breaks = seq(from= -100, to=350, by=50 ), expand = c(0,0)) + geom_hline(yintercept = 0, linetype="dashed")

idx <- which(sapply(pp$layers, function(l) "PositionJitter" %in% class(l$position)))
#idx <- which(sapply(pp$layers, function(l) "PositionJitterdodge" %in% class(l$position)))

pp$layers[[1]]$geom_params$outlier.alpha = 0
pp$layers[[idx]]$aes_params$alpha <- 0.2
pp$layers[[idx]]$aes_params$size <- 3
pp$layers[[idx]]$position$jitter.width = .15
pp <- ggpar(p = pp, legend.title = "Task Relevant Context", legend = "bottom")
pp

pp <- ggpubr::ggbarplot(RT.Diff, x = "Condition", y = "Flanker",
                        color = "Task_Relevant_Context", palette =c("black","black"), label.rectangle = TRUE,
                        fill = c("#276478","#CA3542", "#276478","#CA3542", "#276478","#CA3542"),
                        add = c("mean_se", "jitter"),
                        #add.params = list(size = 1, width = ),
                        shape = "Task_Relevant_Context",
                        #size = 1.3,
                        position = position_dodge(.7)) +
      ggpubr::rremove("legend")

pp    <- pp + scale_y_continuous(limits = c(-125, 375), breaks = seq(from= -100, to=350, by=50 ), expand = c(0,0)) + geom_hline(yintercept = 0)

#idx <- which(sapply(pp$layers, function(l) "PositionJitter" %in% class(l$position)))
idx <- which(sapply(pp$layers, function(l) "PositionJitterdodge" %in% class(l$position)))

#pp$layers[[1]]$aes_params$colour = c("black","grey", "black","grey","black","grey")
#pp$layers[[1]]$position$dodge.width = .4

pp$layers[[1]]$geom_params$outlier.alpha = 0
pp$layers[[idx]]$aes_params$alpha <- 0.2
pp$layers[[idx]]$aes_params$size <- 3
pp$layers[[idx]]$position$jitter.width = .2
#pp$layers[[idx]]$position$dodge.width = .4
pp

#######


#WRS2::bwtrim(formula = Flanker ~ Condition*Task_Relevant_Context, id = Subject, data = RT.Diff)


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
               legend.background = element_rect(colour = "black", fill = "white", size=1),
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
               legend.background = element_rect(colour = "black", fill = "white", size=1),
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

title <- ggdraw() + draw_label("Experiment 1", fontface='bold')
figure2<-plot_grid(title, figure2, ncol=1, rel_heights=c(0.1, 1)) # rel_heights values control title margins

figure2

ggsave("figure2.pdf", device = "pdf", dpi = 600,
      width = 6.875, height = 4, units = "in") 

#clean up environment
#rm(list=c("ACC.analysis","aov.out","raw_data","RT.Analysis","RT.DF","low_acc","vjoutNR", "RT.Diff"))

