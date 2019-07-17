library(dplyr)
library(tidyr)
library(afex)
library(ggplot2)
library(cowplot)
library(retimes)
library(doParallel)

#As of 2018/12/07 apa_print() requires development version of Papaja
#devtools::install_github("crsh/papaja")
library(papaja)

#devtools::install_github("CrumpLab/conflictPower")
library(conflictPower)


############### load all data sets and summarize ####################
load("data-analysis/raw_data_E1.Rda")
raw_data <-
  raw_data %>% mutate(Subject = as.numeric(as.character(Subject)))
raw_data$PC <- "none"



load("data-analysis/raw_data_E2.Rda")
raw_data_E2$Subject <-
  raw_data_E2$Subject + max(as.numeric(as.character(raw_data$Subject)))
raw_data_E2$Task_Relevant_Context  <- "none"
raw_data_E2$Condition <- "none"
raw_data_E2$Frequency <- "none"


load("data-analysis/raw_data_E3.Rda")
raw_data_E3$Subject <-
  raw_data_E3$Subject + max(as.numeric(as.character(raw_data_E2$Subject)))
raw_data_E3$PC <- "none"
data <- rbind(raw_data, raw_data_E2, raw_data_E3)


# Find subs with < 75% accuracy
low_acc <- data %>%
  mutate(Condition = as.character(Condition)) %>%
  group_by(Subject,
           Condition,
           PC,
           Task_Relevant_Context,
           Frequency,
           Congruency) %>%
  summarise(meanAccuracy = mean(ACC)) %>%
  filter(meanAccuracy < .75) %>%
  .$Subject

N_subjects_removed <- length(unique(low_acc))

### Tag RTs as outliers using van selst and jolicour ###
xsize <- c(1.3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 20, 25, 30, 35, 50, 80)
stds <- c(1.3,1.458,1.68,1.841,1.961,2.05,2.12,2.173,2.22,2.246,2.274,2.31,2.326,2.391,2.41,2.4305,2.45,2.48,2.5)

minMax <- data %>%
  filter(RT < 3000,
         RT > 0,
         ACC == TRUE,
         Subject %in% low_acc == FALSE) %>%
  group_by(Condition,
           PC,
           Subject,
           Order,
           Phase,
           Frequency,
           Task_Relevant_Context,
           Congruency) %>%
  summarize (
    Ntrials = n(),
    cutoff = stds[length(xsize[xsize <= length(RT)])],
    min = mean(RT) - sd(RT) * stds[length(xsize[xsize <= length(RT)])],
    max = mean(RT) + sd(RT) * stds[length(xsize[xsize <= length(RT)])]
  )

new.data <- merge(data, minMax)
new.data$outlier <-
  new.data$RT < new.data$min | new.data$RT > new.data$max

####################


### get ex-gaussian estimates for inc/con across all three experiments
s <- new.data %>%
  filter(RT < 3000,
         RT > 0,
         outlier == FALSE,
         ACC == TRUE,
         Subject %in% low_acc == FALSE) %>%
  group_by(Subject, Congruency) %>%
  summarise(m = mexgauss(RT)[1],
            s = mexgauss(RT)[2],
            t = mexgauss(RT)[3]) %>%
  group_by(Congruency) %>%
  summarize(
    mu = mean(m),
    mu_sd = sd(m),
    sigma = mean(s),
    sigma_sd = sd(s),
    tau = mean(t),
    tau_sd = sd(t)
  )


#### calculate power for range of effect sizes ####

effect.sizes <- c(seq(from = 1, to = 50, by = 1))

## for three-way interaction:
cl <- makeCluster(2)
registerDoParallel(cl)
sim_results <-
  foreach::foreach(i = 1:length(effect.sizes), .combine = cbind) %dopar% conflictPower::pc_modulation_power_fast(
    subjects = 50,
    design = "between",
    A_mc_c_nmst = c(48, s$mu[2] + effect.sizes[i], s$sigma[2], s$tau[2]),
    A_mc_nc_nmst = c(48, s$mu[1], s$sigma[1], s$tau[1]),
    A_mnc_c_nmst = c(48, s$mu[2], s$sigma[2], s$tau[2]),
    A_mnc_nc_nmst = c(48, s$mu[1], s$sigma[1], s$tau[1]),
    B_mc_c_nmst = c(48, s$mu[2], s$sigma[2], s$tau[2]),
    B_mc_nc_nmst = c(48, s$mu[1], s$sigma[1], s$tau[1]),
    B_mnc_c_nmst = c(48, s$mu[2], s$sigma[2], s$tau[2]),
    B_mnc_nc_nmst = c(48, s$mu[1], s$sigma[1], s$tau[1]),
    num_sims = 10000,
    alpha = .05
  )
stopCluster(cl)

results <- data.frame(
  effect = "CSPC x Condition (N = 50/group)",
  sample.size = 50,
  prop.sig = c(sim_results),
  effect.size = seq(1:50)
)


## for two-way interaction (N = 150):
cl <- makeCluster(2)
registerDoParallel(cl)
sim_results2 <-
  foreach::foreach(i = 1:length(effect.sizes), .combine = cbind) %dopar% conflictPower::pc_power_fast(
    subjects = 150,
    mc_c_nmst = c(48, s$mu[2] + effect.sizes[i], s$sigma[2], s$tau[2]),
    mc_nc_nmst = c(48, s$mu[1], s$sigma[1], s$tau[1]),
    mnc_c_nmst = c(48, s$mu[2], s$sigma[2], s$tau[2]),
    mnc_nc_nmst = c(48, s$mu[1], s$sigma[1], s$tau[1]),
    num_sims = 10000,
    alpha = .05
  )
stopCluster(cl)

results2 <- data.frame(
  effect = "CSPC (N = 150)",
  sample.size = 150,
  prop.sig = c(sim_results2),
  effect.size = seq(1:50)
)



## for two-way interaction (N = 50):
cl <- makeCluster(2)
registerDoParallel(cl)
sim_results3 <-
  foreach::foreach(i = 1:length(effect.sizes), .combine = cbind) %dopar% conflictPower::pc_power_fast(
    subjects = 50,
    mc_c_nmst = c(48, s$mu[2] + effect.sizes[i], s$sigma[2], s$tau[2]),
    mc_nc_nmst = c(48, s$mu[1], s$sigma[1], s$tau[1]),
    mnc_c_nmst = c(48, s$mu[2], s$sigma[2], s$tau[2]),
    mnc_nc_nmst = c(48, s$mu[1], s$sigma[1], s$tau[1]),
    num_sims = 10000,
    alpha = .05
  )
stopCluster(cl)

results3 <- data.frame(
  effect = "CSPC (N = 50)",
  sample.size = 50,
  prop.sig = c(sim_results3),
  effect.size = seq(1:50)
)


sim_effectSizes <- rbind(sim_effectSizes,results3)

levels(sim_effectSizes$effect) <- c("CSPC x Condition", "PC (N = 150)", "PC (N = 50)")

save(sim_effectSizes, file="data-analysis/sim_effectSizes.Rda")

#### calculate sample sizes for sample sizes ####
effect.size <- c(15, 20, 25)
sample.size <- c(seq(from=10, to=200, by=10))
results <- data.frame()


for (n in 1:length(sample.size)){
    cl <- makeCluster(2)
    registerDoParallel(cl)
    sim_results <-
      foreach::foreach(i = 1:length(effect.size), .combine = cbind) %dopar% conflictPower::pc_modulation_power_fast(
        subjects = sample.size[n],
        design = "between",
        A_mc_c_nmst = c(48, s$mu[2] + effect.size[i], s$sigma[2], s$tau[2]),
        A_mc_nc_nmst = c(48, s$mu[1], s$sigma[1], s$tau[1]),
        A_mnc_c_nmst = c(48, s$mu[2], s$sigma[2], s$tau[2]),
        A_mnc_nc_nmst = c(48, s$mu[1], s$sigma[1], s$tau[1]),
        B_mc_c_nmst = c(48, s$mu[2], s$sigma[2], s$tau[2]),
        B_mc_nc_nmst = c(48, s$mu[1], s$sigma[1], s$tau[1]),
        B_mnc_c_nmst = c(48, s$mu[2], s$sigma[2], s$tau[2]),
        B_mnc_nc_nmst = c(48, s$mu[1], s$sigma[1], s$tau[1]),
        num_sims = 10000,
        alpha = .05
      )
    stopCluster(cl)
    
    results <- rbind(results, data.frame(
      effect = "AxBxC",
      sample.size = sample.size[n],
      prop.sig = c(sim_results),
      effect.size = effect.size
      
    ))
    
}

for (n in 1:length(sample.size)){
  cl <- makeCluster(2)
  registerDoParallel(cl)
  sim_results <-
    foreach::foreach(i = 1:length(effect.size), .combine = cbind) %dopar% conflictPower::pc_power_fast(
      subjects = sample.size[n],
      mc_c_nmst = c(48, s$mu[2] + effect.size[i], s$sigma[2], s$tau[2]),
      mc_nc_nmst = c(48, s$mu[1], s$sigma[1], s$tau[1]),
      mnc_c_nmst = c(48, s$mu[2], s$sigma[2], s$tau[2]),
      mnc_nc_nmst = c(48, s$mu[1], s$sigma[1], s$tau[1]),
      num_sims = 10000,
      alpha = .05
    )
  stopCluster(cl)
  
  results <- rbind(results, data.frame(
    effect = "PC",
    sample.size = sample.size[n],
    prop.sig = c(sim_results),
    effect.size = effect.size
    
  ))
  
}


results <- results %>%
  mutate(effect.size = factor(effect.size))

sim_sampleSizes <- results
save(sim_sampleSizes, file="data-analysis/sim_sampleSizes.Rda")


