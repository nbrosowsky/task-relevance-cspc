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
  select(Exp, gender, age, handedness, vision, english) %>%
  mutate(age = as.character(age)) %>%
  filter(age != "male") %>%
  mutate(age = as.numeric(age)) %>%
  group_by(Exp) %>%
  mutate(bin = case_when(age < 30 ~ "18-29",
                         age > 29 & age < 40 ~ "30-39",
                         age > 39 & age < 50 ~ "40-49",
                         age > 49 & age < 60 ~ "50-59",
                         age > 59  ~ "> 59",
                         TRUE ~ "No Response")) %>%
  ungroup() %>%
  mutate(bin = as.factor(bin)) %>%
  group_by(Exp, bin)%>%
  summarise(n = n()) %>%
  mutate(freq = round((n / sum(n)*100), digits = 2)) %>%
  complete(bin, fill = list(n = 0, freq = 0)) %>%
  arrange(Exp, factor(bin, levels = c("> 59", "50-59", "40-49", "30-39", "18-29", "No Response"))) %>%
  mutate(bin = as.character(bin))  %>%
  mutate(freq = format(freq, digits = 2, nsmall = 2))

age <- cbind(age[age$Exp == "E1",2:4],age[age$Exp == "E2",3:4],age[age$Exp == "E3",3:4])

gender <- df %>%
  group_by(Exp) %>%
  summarize(Male = table(gender)[[1]],
            Female = table(gender)[[2]],
            `No Response` = table(gender)[[3]]) %>%
  gather(Gender, G_Count, Male:`No Response`) %>%
  group_by(Exp, Gender) %>%
  summarise(n = G_Count) %>%
  mutate(freq = round((n / sum(n)*100), digits = 2)) %>%
  arrange(Exp, factor(Gender, levels = c("Female", "Male","No Response")))  %>%
  mutate(freq = format(freq, digits = 2, nsmall = 2))

gender <- cbind(gender[gender$Exp == "E1",2:4],gender[gender$Exp == "E2",3:4],gender[gender$Exp == "E3",3:4])


handedness <- df %>%
  group_by(Exp) %>%
  summarize(Right = table(handedness)[[1]],
            Left = table(handedness)[[2]],
            Both = table(handedness)[[4]],
            `No Response` = table(handedness)[[3]]) %>%
  gather(Handedness, H_Count, Right:`No Response`) %>%
  group_by(Exp, Handedness) %>%
  summarise(n = H_Count) %>%
  mutate(freq = round((n / sum(n)*100), digits = 2)) %>%
  arrange(Exp, factor(Handedness, levels = c("Left", "Right","Both", "No Response"))) %>%
  mutate(freq = format(freq, digits = 2, nsmall = 2))

handedness <- cbind(handedness[handedness$Exp == "E1",2:4],handedness[handedness$Exp == "E2",3:4],handedness[handedness$Exp == "E3",3:4])


TR_Table_4 <-paste(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\begin{threeparttable}",
  "\\caption{Demographics from Experiments 1 to 3}",
  "\\label{TR_table_4}",
  "\\begin{tabular}{llccccccc}",
  "\\toprule",
  "& \\multicolumn{2}{c}{Exp. 1} & \\multicolumn{2}{c}{Exp. 2} & \\multicolumn{2}{c}{Exp. 3} & \\\\", 
  "\\cmidrule(rl){2-3}",
  "\\cmidrule(rl){4-5}",
  "\\cmidrule(rl){6-7}",
  "\\multicolumn{1}{c}{Age} & \\multicolumn{1}{c}{N} & \\multicolumn{1}{c}{\\%} & \\multicolumn{1}{c}{N} & \\multicolumn{1}{c}{\\%} & \\multicolumn{1}{c}{N} & \\multicolumn{1}{c}{\\%}  \\\\",
  "\\midrule", 
  paste0("\\multicolumn{1}{l}{", paste(age[1,],collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
  paste0("\\multicolumn{1}{l}{", paste(age[2,],collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
  paste0("\\multicolumn{1}{l}{", paste(age[3,],collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
  paste0("\\multicolumn{1}{l}{", paste(age[4,],collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
  paste0("\\multicolumn{1}{l}{", paste(age[5,],collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
  paste0("\\multicolumn{1}{l}{", paste(age[6,],collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
  "& & & & & & & \\\\",
  "\\multicolumn{1}{c}{Gender} & & & & & & \\\\",
  "\\midrule", 
  paste0("\\multicolumn{1}{l}{", paste(gender[1,],collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
  paste0("\\multicolumn{1}{l}{", paste(gender[2,],collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
  paste0("\\multicolumn{1}{l}{", paste(gender[3,],collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
  "& & & & & & & \\\\",
  "\\multicolumn{1}{c}{Handedness} & & & & & & \\\\",
  "\\midrule", 
  paste0("\\multicolumn{1}{l}{", paste(handedness[1,],collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
  paste0("\\multicolumn{1}{l}{", paste(handedness[2,],collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
  paste0("\\multicolumn{1}{l}{", paste(handedness[3,],collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
  paste0("\\multicolumn{1}{l}{", paste(handedness[4,],collapse="} & \\multicolumn{1}{l}{"), "} \\\\"),
  " & & & & & \\\\",
  "\\bottomrule",
  "\\end{tabular}%",
  "\\end{threeparttable}",
  "\\end{table}",
  sep = "\n"
)