library(tidyverse)
library(lme4)
library(here)


data <- readRDS(file = here::here('data.rds'))

## Build models ####
fit0 <- lmer(batAvg ~ 1 
             + GS 
             + salary 
             + cumAwards 
             + soPer 
             + age
             + cumGamesPlayed
             + yearService
             + (1 + yearService|playerID), 
             data = data)