library(data.table)
library(tidyverse)
library(lme4)
library(lmerTest)
library(effectsize)
library(emmeans)

setwd("data_directory")

#subject, state, condition, index
data <- read.csv("data_file")

#example to test attention state (Ex. Fig. 1E, F)
m <- lmer(value ~ state + (1|subN),data)
anova(m)
print(eta_squared(m, partial = TRUE))

emm <- emmeans(m, ~ state)
pairs(emm, adjust = "tukey")
t_to_d(
  t = xxx,
  df_error = xxx
)

#example to test condition(Ex. Fig. 1D)
m <- lmer(value ~ condition + (1|subN),data)
anova(m)
print(eta_squared(m, partial = TRUE))

emm <- emmeans(m, ~ condition)
pairs(emm, adjust = "tukey")
t_to_d(
  t = xxx,
  df_error = xxx
)

#example to test interaction(Ex. Fig. 3A)
m <- lmer(value ~ condition * state + (1|subN),data)
anova(m)
print(eta_squared(m, partial = TRUE))

F_to_eta2(
  f = xxx,
  df = xxx,
  df_error = xxx
)

emm <- emmeans(m, ~ condition)
pairs(emm, adjust = "tukey")

emm <- emmeans(m, ~ state)
pairs(emm, adjust = "tukey")

t_to_d(
  t = xxx,
  df_error = xxx
)
