
## SETUP -------------------
library(dplyr)
library(tidyr)
library(afex)
library(ggplot2)
library(cowplot)
library(forcats)
library(BayesFactor)
#As of 2018/12/07 apa_print() requires development version of Papaja
#devtools::install_github("crsh/papaja")
library(papaja)

set.seed(10000)

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


## Pre-process data ------------------------------------
load("data-analysis/raw_data_E1.Rda")

levels(raw_data$Order) <- c("0% to 100% PC","100% to 0% PC")
raw_data$Order <- factor(raw_data$Order, c("100% to 0% PC","0% to 100% PC" ))
 
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
  summarize(vjoutRT = mean(RT)) %>%
    spread(Congruency, vjoutRT) %>%
    mutate(CE = inc - con)



RT.summary <- RT.DF %>%
  filter(Frequency == "unbiased") %>%
  filter(!is.na(CE)) %>%
  group_by(Order, Task_Relevant_Context) %>%
  summarize(
    n = n(),
    m = mean(CE),
    sd = sd(CE),
    se = sd/sqrt(n)
  )

limits <- aes(ymax = m + se, ymin = m - se)
order_effect_plot <-ggplot(RT.summary,aes(x=Order, y=m, fill=Task_Relevant_Context))+
  geom_bar(stat="identity", position=position_dodge(width=0.9), colour = "black") +
  scale_fill_manual(values=c("#a6611a", "#dfc27d"))+
  geom_errorbar(limits, width = .2, position=position_dodge(width=0.9))+
  coord_cartesian(ylim = c(0,150))+
  scale_y_continuous(breaks=seq(0, 150, 10), expand = c(0,0))+
  theme(axis.text=element_text(size=7.5),
        axis.title=element_text(size=10,face="bold")) +
  labs(title="All Trials")+
  ylab("Congruency Effect (ms)")+
  xlab("Phase Order") +
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

E1_Order_A <- aov_car(CE ~ Task_Relevant_Context*Order + Error(Subject/Task_Relevant_Context), data = RT.DF %>% filter(Frequency == "unbiased"))
E1_Order_A <- print_apa_ci(E1_Order_A)


E1_Order_A_BF = BayesFactor::anovaBF(CE ~ Condition*Task_Relevant_Context + Subject, data = RT.DF %>% filter(Frequency == "unbiased"),
                             whichRandom="Subject", iterations=10000)
pE1_Order_A_BF = print_bf(E1_Order_A_BF)


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
levels(raw_data$Order) <- c("0% to 100% PC","100% to 0% PC")
raw_data$Order <- factor(raw_data$Order, c("100% to 0% PC","0% to 100% PC" ))
raw_data<-raw_data %>%
  mutate(
    PC = case_when(Order == "0% to 100% PC" & Phase == 1 ~ "MI",
                   Order == "0% to 100% PC" & Phase == 2 ~ "MC",
                   Order == "100% to 0% PC" & Phase == 1 ~ "MC",
                   Order == "100% to 0% PC" & Phase == 2 ~ "MI")
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
  summarize(vjoutRT = mean(RT),
            n = n())


# check for missing data
missing <- RT.DF %>%
     filter(!is.nan(vjoutRT)) %>%
     filter(Frequency == "unbiased") %>%
     group_by(Subject,Order,sm_bl,Phase,Congruency) %>%
     summarize(vjoutRT = mean(vjoutRT)) %>%
     spread(Congruency, vjoutRT) %>%
     mutate(Diff = inc - con) %>%
  group_by(Subject) %>%
  filter(is.na(Diff)) %>%
  .$Subject
  

# re-label as the transition from pre-change to post-change
RT.DF <- RT.DF %>%
  mutate(
    Transition = case_when(
      Phase == 1 & sm_bl == "2" ~ "1. pre-change",
      Phase == 2 & sm_bl == "1" ~ "2. post-change",
      TRUE ~ "other"
    )
  )


RT.DF <- RT.DF %>%
  filter(!is.nan(vjoutRT)) %>%
  filter(Frequency == "unbiased",
         Transition != "other",
         Subject%in%missing == FALSE) %>%
  group_by(Subject,Order,Task_Relevant_Context,Congruency) %>%
  summarize(vjoutRT = mean(vjoutRT)) %>%
  spread(Congruency, vjoutRT) %>%
  mutate(CE = inc - con) %>%
  filter(!is.na(CE))

RT.summary <- RT.DF %>%
  group_by(Order,Task_Relevant_Context) %>%
  summarize(
    n = n(),
    m = mean(CE),
    sd = sd(CE),
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
  labs(title="At the Phase Boundary")+
  ylab("Congruency Effect (ms)")+
  xlab("Phase Order") +
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

E1_Order_B <- aov_car(CE ~ Task_Relevant_Context*Order + Error(Subject/Task_Relevant_Context), data = RT.DF)

E1_Order_B <- print_apa_ci(E1_Order_B)
E1_Order_B_BF = BayesFactor::anovaBF(CE ~ Task_Relevant_Context*Order + Subject, data = RT.DF, whichRandom="Subject", iterations = 10000)
pE1_Order_B_BF = print_bf(E1_Order_B_BF)


########## ARRANGE #########
figure2B<-plot_grid(order_effect_plot,NULL,transition_plot, 
                   nrow = 1, 
                   rel_widths = c(1, 0.05, 1),
                   labels = c("A", "", "B"))

#title <- ggdraw() + draw_label("Experiment 1: Phase Order", fontface='bold')
#figure2B <- plot_grid(title, figure2B, ncol=1, rel_heights=c(0.1, 1)) # rel_heights values control title margins


ggsave("figure2B.pdf", device = "pdf", dpi = 600,
       width = 6.875, height = 4, units = "in") 






