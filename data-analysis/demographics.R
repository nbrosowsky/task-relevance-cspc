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

load("data-analysis/demographics_E1.Rda")
load("data-analysis/demographics_E2.Rda")
load("data-analysis/demographics_E3.Rda")
demographics$Exp <- "E1"

df <- rbind(demographics, demographics_E2)
df <- rbind(df, demographics_E3[,1:16])
df$Exp <- as.factor(df$Exp)

age <- df %>%
  filter(age != "undefined",
         age != "") %>%
  select(Exp, gender, age, handedness, vision, english) %>%
  mutate(age = as.character(age)) %>%
  filter(age != "male") %>%
  mutate(age = as.numeric(age)) %>%
  group_by(Exp) %>%
  summarize(
    mean = mean(age),
    median = median(age),
    sd = sd (age)
  )

age <- left_join(age, df %>% 
  filter(age == "undefined" | age == "" | age == "male" | is.na(age)) %>%
  group_by(Exp) %>%
  summarize(
    `No Response` = n()
  ), by = c("Exp"))

age[is.na(age$`No Response`),]$`No Response` <- 0

gender <- df %>%
  group_by(Exp) %>%
  summarize(Male = table(gender)[[1]],
            Female = table(gender)[[2]],
            `No Response` = table(gender)[[3]])

handedness <- df %>%
  group_by(Exp) %>%
  summarize(Right = table(handedness)[[1]],
            Left = table(handedness)[[2]],
            `No Response` = table(handedness)[[3]],
            Both = table(handedness)[[4]])
