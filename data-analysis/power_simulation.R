
library(dplyr)
library(tidyr)
library(afex)
library(ggplot2)
library(cowplot)
#As of 2018/12/07 apa_print() requires development version of Papaja
#devtools::install_github("crsh/papaja")
library(papaja)

######### FUNCTIONS ##########3

### Outlier removal function
vjoutNR <- function(x,n) {
    if(length(x) == 1){return (x)}
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

### Run single simulation 
run_sim <- function(pop_mean = NULL,
                    pop_sd = NULL,
                    effect_estimate = "B",
                    effect_dist = "3/3",
                    effect_size = 5,
                    n_subs = 20,
                    upper = pop_mean + 2 * pop_sd,
                    lower = pop_mean - 2 * pop_sd) {
    
    n_subs <- n_subs * 3
    
    # sample some rts
    # over-sample and discard overflow
    rts <- c()
    while (length(rts) < (n_subs * 2)) {
        # over sample d
        t <- rnorm((n_subs * 3),mean = pop_mean,sd = pop_sd)
        rts <- t[t > lower & t < upper]
    }
    rts <- rts[1:(n_subs * 2)]
    
    ### Add effect
    if (effect_dist == "3/3") {
        rts[1:(length(rts) / 2)] <- rts[1:(length(rts) / 2)] + effect_size
    }
    
    if (effect_dist == "2/3") {
        rts[1:(length(rts) / 3)] <- rts[1:(length(rts) / 3)] + effect_size
    }
    
    if (effect_dist ==  "1/3") {
        rts[1:(length(rts) / 6)] <- rts[1:(length(rts) / 6)] + effect_size
    }
    #####
    
    ##### create df
    df <- data.frame(
        Subject = rep(c(rep(1:n_subs)), 2),
        Level = rep(c("1", "2"), each = n_subs),
        Condition = rep(c(
            rep("A", n_subs / 3), rep("B", n_subs / 3), rep("C", n_subs / 3)
        ), 2),
        RTs = rts
    )
    
    df %>%
        group_by(Condition, Level) %>%
        summarize(RT = mean(RTs))
    ##### get pvalue
    pvalue<-c()
    
    if (effect_estimate == "A") {
        suppressMessages(
            pvalue <-
                aov_car(
                    RTs ~ Condition * Level + Error(Subject / Level),
                    data = df
                )$anova_table$`Pr(>F)`[1]
        )
        
    }
    
    if (effect_estimate == "B") {
        suppressMessages(
            pvalue <-
                aov_car(
                    RTs ~ Condition * Level + Error(Subject / Level),
                    data = df
                )$anova_table$`Pr(>F)`[2]
        )
        
    }
    
    if (effect_estimate == "AxB") {
        suppressMessages(
            pvalue <-
                aov_car(
                    RTs ~ Condition * Level + Error(Subject / Level),
                    data = df
                )$anova_table$`Pr(>F)`[3]
        )
        
    }
    
    
    return(pvalue)
}


############### load all data sets and summarize ####################
load("data-analysis/raw_data_E1.Rda")
raw_data <- raw_data %>% mutate(Subject = as.numeric(as.character(Subject)))
raw_data$PC <- "none"


load("data-analysis/raw_data_E2.Rda")
raw_data_E2$Subject <- raw_data_E2$Subject + max(as.numeric(as.character(raw_data$Subject)))
raw_data_E2$Task_Relevant_Context  <- "none"
raw_data_E2$Condition <- "none"
raw_data_E2$Frequency <- "non"
raw_data_E2$Exp <- "E2"


load("data-analysis/raw_data_E3.Rda")
raw_data_E3$Subject <- raw_data_E3$Subject + max(as.numeric(as.character(raw_data_E2$Subject)))
raw_data_E3$PC <- "none"
data <- rbind(raw_data,raw_data_E2,raw_data_E3)


# Find subs with < 75% accuracy
low_acc <- data %>%
    mutate(Condition = as.character(Condition))%>%
    group_by(Subject,Condition,PC,Task_Relevant_Context,Frequency,Congruency) %>%
    summarise(meanAccuracy = mean(ACC)) %>%
    filter(meanAccuracy < .75) %>%
    .$Subject

N_subjects_removed<-length(unique(low_acc))

## summarise by subject
RT.DF <- data %>%
    filter(
        RT < 3000,
        RT > 0,
        ACC == TRUE,
        Subject%in%low_acc == FALSE
    ) %>%
    group_by(Condition,PC,Subject,Order,Phase,Frequency,Task_Relevant_Context,Congruency)%>%
    summarise(vjoutRT = vjoutNR(RT,1)) %>%
    ungroup()%>%
    select(Subject, Congruency, vjoutRT)

####### GRAPH FLANKER EFFECTS #########
RT.sum<-RT.DF %>%
    ungroup()%>%
    group_by(Subject,Congruency) %>%
    summarise(
        RT = mean(vjoutRT)
    ) %>%
    spread(Congruency,RT) %>%
    mutate(Diff = inc - con) %>%
    select(-con:-inc)%>%
    ungroup() %>%
    summarise(
        N = n(),
        Flanker = mean(Diff),
        sd = sd(Diff),
        se = sd/sqrt(N)
    )


############## Run simulations ##############


# Simulations 1-3: Effect sizes
toTest <- c(seq(from = 10, to = 40, by = 1))
n_sims <- 10

# Sim 1. find proportion p < .05 for 2x3 interaction 
# when two conditions show effect and one does not (2/3)
# 10 ms to 40 ms effect sizes


sim <- matrix(NA, nrow = length(toTest), ncol = 3)
for (i in seq(1:length(toTest))){
    
   pvalues <- replicate(n_sims, run_sim(pop_mean = RT.sum$Flanker, 
                                      pop_sd = RT.sum$sd, 
                                      effect_estimate = "AxB",
                                      effect_dist = "2/3",
                                      effect_size = toTest[i])
    )
    
   sim[i,1] <- length(pvalues[pvalues < .05]) / n_sims  
   sim[i,2:3] <- prop.test(length(pvalues[pvalues < .05]), n_sims)$conf.int[1:2]
}

sim <- cbind(sim, "AxB")
sim <- cbind(sim, "2/3")
sim <- cbind(sim, toTest)
sim <- cbind(sim, 50)

all_sims <- sim

# Sim 2. find proportion p < .05 for 2x3 interaction 
# when one condition show effect and two do not (1/3)
# 10 ms to 40 ms effect sizees

sim <- matrix(NA, nrow = length(toTest), ncol = 3)
for (i in seq(1:length(toTest))){
    
    pvalues <- replicate(n_sims, run_sim(pop_mean = RT.sum$Flanker, 
                                         pop_sd = RT.sum$sd, 
                                         effect_estimate = "AxB",
                                         effect_dist = "1/3",
                                         effect_size = toTest[i])
    )
    
    sim[i,1] <- length(pvalues[pvalues < .05]) / n_sims  
    sim[i,2:3] <- prop.test(length(pvalues[pvalues < .05]), n_sims)$conf.int[1:2]
}

sim <- cbind(sim, "AxB")
sim <- cbind(sim, "1/3")
sim <- cbind(sim, toTest)
sim <- cbind(sim, 50)

all_sims <- rbind(all_sims, sim)

# Sim 3. find proportion p < .05 for main effect 
# when all three conditions show effect (3/3)
# 10 ms to 40 ms effect sizees

sim <- matrix(NA, nrow = length(toTest), ncol = 3)
for (i in seq(1:length(toTest))){
    
    pvalues <- replicate(n_sims, run_sim(pop_mean = RT.sum$Flanker, 
                                         pop_sd = RT.sum$sd, 
                                         effect_estimate = "B",
                                         effect_dist = "3/3",
                                         effect_size = toTest[i])
    )
    
    sim[i,1] <- length(pvalues[pvalues < .05]) / n_sims  
    sim[i,2:3] <- prop.test(length(pvalues[pvalues < .05]), n_sims)$conf.int[1:2]
}
sim <- cbind(sim, "B")
sim <- cbind(sim, "3/3")
sim <- cbind(sim, toTest)
sim <- cbind(sim, 50)

all_sims <- rbind(all_sims, sim)


# Simulations 4-6: Sample Sizes
toTest <- c(seq(from = 50, to = 300, by = 10))
n_sims <- 10

# Sim 4. find proportion p < .05 for 2x3 interaction 
# when two conditions show effect and one does not (2/3)
# 10 ms to 40 ms effect sizes


sim <- matrix(NA, nrow = length(toTest), ncol = 3)
for (i in seq(1:length(toTest))){
    
    pvalues <- replicate(n_sims, run_sim(pop_mean = RT.sum$Flanker, 
                                         pop_sd = RT.sum$sd, 
                                         effect_estimate = "AxB",
                                         effect_dist = "2/3",
                                         effect_size = 20,
                                         n_subs = toTest[i])
    )
    
    sim[i,1] <- length(pvalues[pvalues < .05]) / n_sims  
    sim[i,2:3] <- prop.test(length(pvalues[pvalues < .05]), n_sims)$conf.int[1:2]
}

sim <- cbind(sim, "AxB")
sim <- cbind(sim, "2/3")
sim <- cbind(sim, 20)
sim <- cbind(sim, toTest)

all_sims <- rbind(all_sims, sim)


# Sim 5. find sample sizes 
# when one condition show effect and two do not (1/3)
# 20 ms effect

sim <- matrix(NA, nrow = length(toTest), ncol = 3)
for (i in seq(1:length(toTest))){
    
    pvalues <- replicate(n_sims, run_sim(pop_mean = RT.sum$Flanker, 
                                         pop_sd = RT.sum$sd, 
                                         effect_estimate = "AxB",
                                         effect_dist = "1/3",
                                         effect_size = 20,
                                         n_subs = toTest[i])
    )
    
    sim[i,1] <- length(pvalues[pvalues < .05]) / n_sims  
    sim[i,2:3] <- prop.test(length(pvalues[pvalues < .05]), n_sims)$conf.int[1:2]
}

sim <- cbind(sim, "AxB")
sim <- cbind(sim, "2/3")
sim <- cbind(sim, 20)
sim <- cbind(sim, toTest)

all_sims <- rbind(all_sims, sim)


# Sim 6. find sample sizes
# when all three conditions show effect (3/3)
# 20 ms effect

sim <- matrix(NA, nrow = length(toTest), ncol = 3)
for (i in seq(1:length(toTest))){
    
    pvalues <- replicate(n_sims, run_sim(pop_mean = RT.sum$Flanker, 
                                         pop_sd = RT.sum$sd, 
                                         effect_estimate = "AxB",
                                         effect_dist = "3/3",
                                         effect_size = 20,
                                         n_subs = toTest[i])
    )
    
    sim[i,1] <- length(pvalues[pvalues < .05]) / n_sims  
    sim[i,2:3] <- prop.test(length(pvalues[pvalues < .05]), n_sims)$conf.int[1:2]
}

sim <- cbind(sim, "B")
sim <- cbind(sim, "3/3")
sim <- cbind(sim, 20)
sim <- cbind(sim, toTest)

all_sims <- rbind(all_sims, sim)


s<-replicate(1000, run_sim(pop_mean = RT.sum$Flanker, 
                        pop_sd = RT.sum$sd, 
                        effect_estimate = "AxB",
                        effect_size = 25))


