
## SETUP -------------------
library(dplyr)
library(tidyr)
library(afex)
library(ggplot2)
library(cowplot)
library(forcats)
#As of 2018/12/07 apa_print() requires development version of Papaja
#devtools::install_github("crsh/papaja")
library(papaja)

# van selst and jolicour outlier removal (non-recursive)
vjoutNR <- function(x,n) {
  xm <- mean(x)
  xsize <- c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 20, 
             25, 30, 35, 50, 80)
  stds <- c(1.3, 1.458, 1.68, 1.841, 1.961, 2.05, 2.12, 2.173, 
            2.22, 2.246, 2.274, 2.31, 2.326, 2.391, 2.41, 2.4305, 
            2.45, 2.48, 2.5)
  stdindex <- length(xsize[xsize <= length(x)])
  removed <- x[x > xm+sd(x)*stds[stdindex]]
  removed <- c(removed, x[x < xm-(sd(x)*stds[stdindex])])
  
  retained <- x[x < xm+sd(x)*stds[stdindex]]
  retained <- retained[retained > xm - (sd(x)*stds[stdindex])]
  
  proportionRemoved <- length(retained)/length(x)
  finaldata<-list(
    mean(retained),
    1-proportionRemoved,
    retained,
    removed
  )
  return(finaldata[[n]])
}

## Pre-process data ------------------------------------
load("data-analysis/raw_data_E1.Rda")

levels(raw_data$Order) <- c("MI-MC","MC-MI")
raw_data$Order <- factor(raw_data$Order, c("MC-MI","MI-MC" ))
 
# Find subs with < 75% accuracy
  low_acc <- raw_data %>%
    group_by(Subject,Condition,Task_Relevant_Context,Frequency,Congruency) %>%
    summarise(meanAccuracy = mean(ACC)) %>%
    filter(meanAccuracy < .75) %>%
    .$Subject
  
  N_subjects_removed<-length(unique(low_acc))


## Exploratory analysis: Order assymmetry ----------------------------------------

# summarize by subject
RT.DF <- raw_data %>%
  filter(
    RT < 3000,
    RT > 0,
    ACC == TRUE,
    Subject%in%low_acc == FALSE
  ) %>%
  group_by(Condition,Subject,Order,Phase,Block,Frequency,Task_Relevant_Context,Congruency)%>%
  mutate(outlier = RT%in%vjoutNR(RT,4)) %>%
  filter(outlier == FALSE) %>%
  group_by(Condition,Subject,Order,Phase,Frequency,Task_Relevant_Context,Congruency)%>%
  summarize(vjoutRT = mean(RT))



RT.summary <- RT.DF %>%
  filter(!is.nan(vjoutRT)) %>%
  filter(Frequency == "unbiased") %>%
  group_by(Subject,Order,Task_Relevant_Context,Congruency) %>%
  summarize(vjoutRT = mean(vjoutRT)) %>%
  spread(Congruency, vjoutRT) %>%
  mutate(Diff = inc - con) %>%
  filter(!is.na(Diff)) %>%
  group_by(Order, Task_Relevant_Context) %>%
  summarize(
    n = n(),
    m = mean(Diff),
    sd = sd(Diff),
    se = sd/sqrt(n)
  )

limits <- aes(ymax = m + se, ymin = m - se)
order_effect_plot <-ggplot(RT.summary,aes(x=Order, y=m, fill=Task_Relevant_Context))+
  geom_bar(stat="identity", position=position_dodge(width=0.9), colour = "black") +
  scale_fill_manual(values=c("#018571", "#80cdc1"))+
  geom_errorbar(limits, width = .2, position=position_dodge(width=0.9))+
  coord_cartesian(ylim = c(0,150))+
  scale_y_continuous(breaks=seq(0, 150, 10), expand = c(0,0))+
  theme(axis.text=element_text(size=7.5),
        axis.title=element_text(size=10,face="bold")) +
  labs(title="All Trials")+
  ylab("Congruency Effect (ms)")+
  theme(       legend.position=c(1,1),
               legend.justification = c(1,1),
               legend.direction = "horizontal",
               legend.background = element_rect(colour = "black", fill = "white", size=1),
               #legend.box.background = element_rect(colour = "black"),
               legend.margin = margin(t = .1, r = .10, b = .05, l = .1, unit = "cm"),
               legend.text = element_text(size = 8),
               legend.title = element_text(size = 8),
               plot.title = element_text(face = "italic", size = 12, hjust = 0)
               
  ) +
  guides(fill=guide_legend(title="Task-Relevant Context",title.position = "top"))

#order_effect_plot

E1_Order_A <- aov_car(vjoutRT ~ Congruency*Task_Relevant_Context*Order + Error(Subject/Congruency*Task_Relevant_Context), data = RT.DF %>% filter(Frequency == "unbiased"))
E1_Order_A <- apa_print(E1_Order_A, es = "pes")$full_result

# 
# RT.summary <- RT.DF %>%
#   filter(!is.nan(vjoutRT)) %>%
#   filter(Frequency == "unbiased") %>%
#   group_by(Subject,Order,Phase,Congruency) %>%
#   summarize(vjoutRT = mean(vjoutRT)) %>%
#   spread(Congruency, vjoutRT) %>%
#   mutate(Diff = inc - con) %>%
#   filter(!is.na(Diff)) %>%
#   group_by(Order, Phase) %>%
#   summarize(
#     n = n(),
#     m = mean(Diff),
#     sd = sd(Diff),
#     se = sd/sqrt(n)
#   ) %>%
#   arrange(desc(Order))
# 
# limits <- aes(ymax = m + se, ymin = m - se)
# ggplot(RT.summary,aes(x=Order, y=m, fill=Phase))+
#   geom_bar(stat="identity", position=position_dodge(width=0.9), colour = "black") + 
#   scale_fill_manual(values=c("#018571", "#80cdc1"))+
#   geom_errorbar(limits, width = .2, position=position_dodge(width=0.9))+
#   coord_cartesian(ylim = c(0,150))+
#   scale_y_continuous(breaks=seq(0, 150, 10), expand = c(0,0))+
#   theme(axis.text=element_text(size=7.5),
#         axis.title=element_text(size=10,face="bold")) +
#   labs(title="")+
#   ylab("Congruency Effect (ms)")+
#   theme(       legend.position=c(1,1),
#                legend.justification = c(1,1),
#                legend.direction = "horizontal",
#                legend.background = element_rect(colour = "black", fill = "white", size=1),
#                legend.box.background = element_rect(colour = "black"),
#                legend.margin = margin(t = .1, r = .10, b = .05, l = .1, unit = "cm"),
#                legend.text = element_text(size = 8),
#                legend.title = element_text(size = 8)
#   ) +
#   guides(fill=guide_legend(title="Phase",title.position = "top"))
# 
# aov_car(vjoutRT ~ Congruency*Task_Relevant_Context*Order + Error(Subject/Congruency*Task_Relevant_Context), data = RT.DF %>% filter(Frequency == "unbiased"))


## Exploratory analysis: Phase boundary --------------------------------------------------

# Divide each phase into two blocks (24 unbiased trials each)
raw_data <- raw_data %>%
  group_by(Subject,Phase) %>%
  mutate(new_Trial = 1:n()) %>%
  mutate(sm_bl = case_when(
    new_Trial <= 72 ~ "1",
    new_Trial <= 144 ~ "2"
    )
  )



# re-label order and PC phases
levels(raw_data$Order) <- c("MI-MC","MC-MI")
raw_data$Order <- factor(raw_data$Order, c("MC-MI","MI-MC" ))
raw_data<-raw_data %>%
  mutate(
    PC = case_when(Order == "MI-MC" & Phase == 1 ~ "MI",
                   Order == "MI-MC" & Phase == 2 ~ "MC",
                   Order == "MC-MI" & Phase == 1 ~ "MC",
                   Order == "MC-MI" & Phase == 2 ~ "MI")
  )



# summarize by subject
RT.DF <- raw_data %>%
  filter(
    RT < 3000,
    RT > 0,
    ACC == TRUE,
    Subject%in%low_acc == FALSE
  ) %>%
  group_by(Condition,Subject,Order,Phase,Block,Frequency,Task_Relevant_Context,Congruency)%>%
  mutate(outlier = RT%in%vjoutNR(RT,4)) %>%
  filter(outlier == FALSE) %>%
  group_by(Condition,Subject,Order,Phase,Frequency,Task_Relevant_Context,sm_bl,Congruency)%>%
  summarize(vjoutRT = mean(RT))



RT.summary <- RT.DF %>%
     filter(!is.nan(vjoutRT)) %>%
     filter(Frequency == "unbiased") %>%
     group_by(Subject,Order,sm_bl,Phase,Congruency) %>%
     summarize(vjoutRT = mean(vjoutRT)) %>%
     spread(Congruency, vjoutRT) %>%
     mutate(Diff = inc - con) 

## check for missing data and remove
# with the split-half analysis, no subjects are removed
missing <- RT.summary %>%
  group_by(Subject) %>%
  filter(is.na(Diff)) %>%
  .$Subject
  
  
RT.summary <- RT.summary %>%
  filter(Subject%in%missing == FALSE) %>%
     group_by(Order,Phase,sm_bl) %>%
     summarize(
         n = n(),
         m = mean(Diff),
         sd = sd(Diff),
         se = sd/sqrt(n)
       )

# 
# aov_car(vjoutRT ~ Congruency*Task_Relevant_Context*Order + Error(Subject/Congruency*Task_Relevant_Context), data = RT.DF %>% filter(Frequency == "biased"))
# 
# 
# ggplot(RT.summary,aes(x=sm_bl, y=m))+
#      #ggtitle("Exp. 1")+
#      geom_errorbar(limits, width = .2, position=position_dodge(width=0.9), color = "dark grey")+
#      geom_line(aes(group = 1), show.legend = FALSE)+
#      geom_point(aes(group = sm_bl, shape=Order), show.legend = FALSE, size=2) + 
#      theme_classic(base_size=21)+
#      facet_grid(Order~Phase)  

RT.summary <- RT.DF %>%
  filter(!is.nan(vjoutRT)) %>%
  filter(Frequency == "unbiased") %>%
  group_by(Subject,Order,Phase,Task_Relevant_Context,sm_bl,Congruency) %>%
  summarize(vjoutRT = mean(vjoutRT)) %>%
  spread(Congruency, vjoutRT) %>%
  mutate(Diff = inc - con) %>%
  filter(!is.na(Diff)) %>%
  group_by(Order,Phase,Task_Relevant_Context,sm_bl) %>%
  summarize(
    n = n(),
    m = mean(Diff),
    sd = sd(Diff),
    se = sd/sqrt(n)
  )


# re-label as the transition from pre-change to post-change
RT.DF <- RT.DF %>%
  mutate(
    Transition = case_when(
      Phase == 1 & sm_bl == "2" ~ "1. pre-change",
      Phase == 2 & sm_bl == "1" ~ "2. post-change",
      TRUE ~ "other"
    )
  )
# 
# 
# RT.summary <- RT.DF %>%
#   filter(!is.nan(vjoutRT)) %>%
#   filter(Frequency == "unbiased",
#          Transition != "other",
#          Subject%in%missing == FALSE) %>%
#   group_by(Subject,Order,Task_Relevant_Context,Transition,Congruency) %>%
#   summarize(vjoutRT = mean(vjoutRT)) %>%
#   spread(Congruency, vjoutRT) %>%
#   mutate(Diff = inc - con) %>%
#   filter(!is.na(Diff)) %>%
#   group_by(Order,Task_Relevant_Context,Transition) %>%
#   summarize(
#     n = n(),
#     m = mean(Diff),
#     sd = sd(Diff),
#     se = sd/sqrt(n)
#   )
# 
# 
# 
# limits <- aes(ymax = m + se, ymin = m - se)
# ggplot(RT.summary,aes(x=Order, y=m, fill=Transition))+
#   scale_fill_manual(values=c("#018571", "#80cdc1"))+
#   geom_bar(stat="identity", position=position_dodge(width=0.9), colour = "black") + 
#   geom_errorbar(limits, width = .2, position=position_dodge(width=0.9))+
#   coord_cartesian(ylim = c(0,150))+
#   scale_y_continuous(breaks=seq(0, 150, 10), expand = c(0,0))+
#   theme(axis.text=element_text(size=7.5),
#         axis.title=element_text(size=10,face="bold")) +
#   labs(title="")+
#   ylab("Congruency Effect (ms)")+
#   theme(       legend.position=c(1,1),
#                legend.justification = c(1,1),
#                legend.direction = "horizontal",
#                legend.background = element_rect(colour = "black", fill = "white", size=1),
#                legend.box.background = element_rect(colour = "black"),
#                legend.margin = margin(t = .1, r = .10, b = .05, l = .1, unit = "cm"),
#                legend.text = element_text(size = 8),
#                legend.title = element_text(size = 8)
#   ) +
#   guides(fill=guide_legend(title="Transition",title.position = "top"))
# 
# aov_car(vjoutRT ~ Congruency*Transition*Order + Error(Subject/Congruency*Transition), data = RT.DF %>% filter(Frequency == "unbiased", 
#                                                                                               Transition != "other",
#                                                                                               Subject%in%missing == FALSE
#                                                                                               ))

# 
# aov_car(vjoutRT ~ Congruency*Transition + Error(Subject/Congruency*Transition), 
#         data = RT.DF %>% filter(Frequency == "unbiased", 
#                                 Transition != "other",
#                                 Subject%in%missing == FALSE,
#                                 Order == "MC-MI"
#                                 )
#         )





RT.summary <- RT.DF %>%
  filter(!is.nan(vjoutRT)) %>%
  filter(Frequency == "unbiased",
         Transition != "other",
         Subject%in%missing == FALSE) %>%
  group_by(Subject,Order,Task_Relevant_Context,Congruency) %>%
  summarize(vjoutRT = mean(vjoutRT)) %>%
  spread(Congruency, vjoutRT) %>%
  mutate(Diff = inc - con) %>%
  filter(!is.na(Diff)) %>%
  group_by(Order,Task_Relevant_Context) %>%
  summarize(
    n = n(),
    m = mean(Diff),
    sd = sd(Diff),
    se = sd/sqrt(n)
  )



limits <- aes(ymax = m + se, ymin = m - se)
transition_plot <- ggplot(RT.summary,aes(x=Order, y=m, fill=Task_Relevant_Context))+
  geom_bar(stat="identity", position=position_dodge(width=0.9), colour = "black") + 
  scale_fill_manual(values=c("#a6611a", "#dfc27d"))+
  geom_errorbar(limits, width = .2, position=position_dodge(width=0.9))+
  coord_cartesian(ylim = c(0,150))+
  scale_y_continuous(breaks=seq(0, 150, 10), expand = c(0,0))+
  theme(axis.text=element_text(size=7.5),
        axis.title=element_text(size=10,face="bold")) +
  labs(title="At Phase Boundary")+
  ylab("Congruency Effect (ms)")+
  theme(       legend.position=c(1,1),
               legend.justification = c(1,1),
               legend.direction = "horizontal",
               legend.background = element_rect(colour = "black", fill = "white", size=1),
               #legend.box.background = element_rect(colour = "black"),
               legend.margin = margin(t = .1, r = .10, b = .05, l = .1, unit = "cm"),
               legend.text = element_text(size = 8),
               legend.title = element_text(size = 8),
               plot.title = element_text(face = "italic", size = 12, hjust = 0)
  ) +
  guides(fill=guide_legend(title="Task Relevant Context",title.position = "top"))

E1_Order_B <- aov_car(vjoutRT ~ Congruency*Task_Relevant_Context*Order + Error(Subject/Congruency*Task_Relevant_Context), data = RT.DF %>% filter(Frequency == "unbiased", 
                                                                                                              Transition != "other",
                                                                                                              Subject%in%missing == FALSE
))

E1_Order_B <- apa_print(E1_Order_B, es = "pes")$full_result


# aov_car(vjoutRT ~ Congruency*Transition + Error(Subject/Congruency*Transition), 
#         data = RT.DF %>% filter(Frequency == "unbiased", 
#                                 Transition != "other",
#                                 Subject%in%missing == FALSE,
#                                 Order == "MC-MI"
#         )
# )


## Exploratory analysis:  Biased items w/ order

# summarize by subject
# RT.DF <- raw_data %>%
#   filter(
#     RT < 3000,
#     RT > 0,
#     ACC == TRUE,
#     Subject%in%low_acc == FALSE
#   ) %>%
#   group_by(Condition,Subject,Order,Phase,Block,Frequency,Task_Relevant_Context,Congruency)%>%
#   mutate(outlier = RT%in%vjoutNR(RT,4)) %>%
#   filter(outlier == FALSE) %>%
#   group_by(Condition,Subject,Order,Phase,Frequency,Task_Relevant_Context,Congruency)%>%
#   summarize(vjoutRT = mean(RT))
# 
# 
# 
# RT.summary <- RT.DF %>%
#   filter(!is.nan(vjoutRT)) %>%
#   filter(Frequency == "biased") %>%
#   group_by(Subject,Order,Task_Relevant_Context,Congruency) %>%
#   summarize(vjoutRT = mean(vjoutRT)) %>%
#   group_by(Order,Task_Relevant_Context,Congruency) %>%
#   summarize(
#     n = n(),
#     m = mean(vjoutRT),
#     sd = sd(vjoutRT),
#     se = sd/sqrt(n)
#   )
# 
# limits <- aes(ymax = m + se, ymin = m - se)
# ggplot(RT.summary,aes(x=Task_Relevant_Context, y=m, fill=Congruency))+
#   geom_bar(stat="identity", position=position_dodge(width=0.9), colour = "black") + 
#   scale_fill_manual(values=c("gray60", "white"))+
#   geom_errorbar(limits, width = .2, position=position_dodge(width=0.9))+
#   coord_cartesian(ylim = c(500,900))+
#   scale_y_continuous(breaks=seq(500, 900, 20), expand = c(0,0))+
#   theme(axis.text=element_text(size=7.5),
#         axis.title=element_text(size=10,face="bold")) +
#   labs(title="")+
#   ylab("Congruency Effect (ms)")+
#   theme(       legend.position=c(1,1),
#                legend.justification = c(1,1),
#                legend.direction = "horizontal",
#                legend.background = element_rect(colour = "black", fill = "white", size=1),
#                legend.box.background = element_rect(colour = "black"),
#                legend.margin = margin(t = .1, r = .10, b = .05, l = .1, unit = "cm"),
#                legend.text = element_text(size = 8),
#                legend.title = element_text(size = 8)
#   ) +
#   #guides(fill=guide_legend(title="Congruency",title.position = "top")) +
#   facet_wrap(~Order)
# 
# 
# aov_car(vjoutRT ~ Congruency*Task_Relevant_Context*Order + Error(Subject/Congruency*Task_Relevant_Context), data = RT.DF %>% filter(Frequency == "biased"))



########## ARRANGE #########
figure2B<-plot_grid(order_effect_plot,NULL,transition_plot, 
                   nrow = 1, 
                   rel_widths = c(1, 0.05, 1),
                   labels = c("A", "", "B"))

title <- ggdraw() + draw_label("Experiment 1: Phase Order", fontface='bold')
figure2B<-plot_grid(title, figure2B, ncol=1, rel_heights=c(0.1, 1)) # rel_heights values control title margins
 
figure2B

ggsave("figure2B.pdf", device = "pdf", dpi = 600,
       width = 6.875, height = 4, units = "in") 
