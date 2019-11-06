library(tidyverse)
library(lme4)
library(lubridate)
library(here)


data <- readRDS(file = here::here('data.rds'))

## What does a typical trend look like for batting average over the course of a career?

data %>% 
  ggplot(., aes(yearID, batAvg, col = playerID)) +
  geom_line() +
  theme(legend.position = "none")

str(data$debutDate)
year(data$debutDate[1])
year(data$debutDate)

## take (stratefied) samples and look at 5 year intervals
## credit for stratefied samples to https://gist.github.com/ramhiser/8b5ffd0ffbfbf1f49e71bbbd330bf72d
scaleFUN <- function(x) sprintf("%.0f", x)
scaleFUNy <- function(x) sprintf("%.3f", x)
set.seed(12345)
data %>% 
  mutate(debutWindow = case_when(year(debutDate) >= 1993 & year(debutDate) < 1998 ~ "[1993, 1998)",
                                 year(debutDate) >= 1998 & year(debutDate) < 2003 ~ "[1998, 2003)",
                                 year(debutDate) >= 2003 & year(debutDate) < 2008 ~ "[2003, 2008)",
                                 year(debutDate) >= 2008 & year(debutDate) < 2013 ~ "[2008, 2013)",
                                 year(debutDate) >= 2013 ~ "[2013, and after)",
                                 TRUE ~ "[NA]")
         ) %>% 
  mutate(maxYears = max(yearService)) %>% 
  filter(maxYears >= 5) %>% 
  group_by(debutWindow) %>% 
  mutate(num_rows = n()) %>% 
  sample_frac(0.1, weight=num_rows) %>%
  ungroup() %>% # View()
  ggplot(., aes(yearID, batAvg, col = playerID)) +
  geom_line() +
  facet_wrap(~debutWindow,scales = "free_x") +
  theme(legend.position = "none") +
  scale_x_continuous(labels = scaleFUN) +
  scale_y_continuous(labels=scaleFUNy) +
  labs(title = "Sample batting avg paths over time",
       subtitle = "Facetted by debut year interval")

## Use a stratefied sample of 20% from every debutWindow to keep as a testing sample then
## use the remaining for a train sample.
## Study only players with at least 5 years of service
## Replace missing salary with min salary for that playerID
set.seed(12345)
trainIDs <- data %>% 
  mutate(debutWindow = case_when(year(debutDate) >= 1993 & year(debutDate) < 1998 ~ "[1993, 1998)",
                                 year(debutDate) >= 1998 & year(debutDate) < 2003 ~ "[1998, 2003)",
                                 year(debutDate) >= 2003 & year(debutDate) < 2008 ~ "[2003, 2008)",
                                 year(debutDate) >= 2008 & year(debutDate) < 2013 ~ "[2008, 2013)",
                                 year(debutDate) >= 2013 ~ "[2013, and after)",
                                 TRUE ~ "[NA]")
  ) %>% 
  group_by(playerID) %>% 
  mutate(maxYears = max(yearService)) %>% 
  ungroup() %>% 
  filter(yearService == maxYears) %>% 
  filter(maxYears >= 5) %>% 
  group_by(debutWindow) %>% 
  mutate(num_rows = n()) %>% 
  sample_frac(0.8, weight=num_rows) %>%
  ungroup() 
train <- data %>% 
  group_by(playerID) %>% 
  mutate(maxYearService = max(yearService),
         minSalary = min(salary, na.rm=TRUE),
         lagSalary = lag(salary),
         estSalary0 = case_when(is.na(salary) & yearService==1 ~ as.numeric(minSalary),
                               is.na(salary) & yearService>1 ~ as.numeric(lagSalary),
                               TRUE ~ as.numeric(salary)),
         estSalary = if_else(is.na(estSalary0) & yearService == maxYearService, lag(estSalary0), estSalary0)
         ) %>% 
  ungroup() %>% 
  # select(playerID, nameFirst, nameLast, yearService, GS, salary, lagSalary,estSalary0, estSalary, cumAwards, soPer, batAvg) %>% 
  select(yearID, playerID, nameFirst, nameLast, yearService, GS, estSalary, cumAwards, soPer, age,cumGamesPlayed, batAvg) %>% 
  # filter(playerID=="abreubo01") %>% View()
  filter(playerID %in% trainIDs$playerID)

testIDs <- data %>% 
  mutate(debutWindow = case_when(year(debutDate) >= 1993 & year(debutDate) < 1998 ~ "[1993, 1998)",
                                 year(debutDate) >= 1998 & year(debutDate) < 2003 ~ "[1998, 2003)",
                                 year(debutDate) >= 2003 & year(debutDate) < 2008 ~ "[2003, 2008)",
                                 year(debutDate) >= 2008 & year(debutDate) < 2013 ~ "[2008, 2013)",
                                 year(debutDate) >= 2013 ~ "[2013, and after)",
                                 TRUE ~ "[NA]")
  ) %>% 
  group_by(playerID) %>% 
  mutate(maxYears = max(yearService)) %>% 
  ungroup() %>% 
  filter(yearService == maxYears) %>% 
  filter(maxYears >= 5) %>% 
  filter(!playerID %in% trainIDs$playerID)
test <- data %>% 
  group_by(playerID) %>% 
  mutate(maxYearService = max(yearService),
         minSalary = min(salary, na.rm=TRUE),
         lagSalary = lag(salary),
         estSalary0 = case_when(is.na(salary) & yearService==1 ~ as.numeric(minSalary),
                                is.na(salary) & yearService>1 ~ as.numeric(lagSalary),
                                TRUE ~ as.numeric(salary)),
         estSalary = if_else(is.na(estSalary0) & yearService == maxYearService, lag(estSalary0), estSalary0)
  ) %>% 
  ungroup() %>% 
  select(yearID, playerID, nameFirst, nameLast, yearService, GS, estSalary, cumAwards, soPer, age,cumGamesPlayed, batAvg) %>% 
  filter(playerID %in% testIDs$playerID)
  
## Build models ####

# Given above plots, maybe a quadratic pattern would be sufficient for the majority of players?
fit0 <- lmer(batAvg ~ 1 
             + GS 
             + estSalary 
             + cumAwards 
             + soPer 
             + age
             + cumGamesPlayed
             + yearService
             + (1 + yearService + I(yearService^2) | playerID), 
             data = train)
summary(fit0)
head(lme4::ranef(fit0)[[1]])
lme4::fixef(fit0)

# estimates are very small for the squared random effects. Let's remove them
# also removed cumGamesPlayed and age since its highly correlated with yearService

fit1 <- lmer(batAvg ~ 1 
             + GS 
             + estSalary 
             + cumAwards 
             + soPer
             + yearService
             + (1 + yearService | playerID), 
             data = train)
summary(fit1)
head(lme4::ranef(fit1)[[1]])
lme4::fixef(fit1)

train$pred1 <- predict(fit1, train, re.form = NULL) # includes all random effects
train %>% 
  select(playerID, nameFirst, nameLast, GS, estSalary, cumAwards, soPer, yearService, batAvg, pred1) %>% 
  filter(is.na(pred1)) %>% 
  View()

# Looks like there's lots of missin salaries in the data. try again without it
fit2 <- lmer(batAvg ~ 1 
             + GS 
             # + estSalary 
             + cumAwards 
             + soPer
             + yearService
             + (1 + yearService | playerID), 
             data = train)
summary(fit2)
head(lme4::ranef(fit2)[[1]])
lme4::fixef(fit2)

train$pred2 <- predict(fit2, train, re.form = NULL) # includes all random effects
train %>% 
  select(yearID, playerID, nameFirst, nameLast, GS, estSalary, cumAwards, soPer, yearService, batAvg, pred2) %>% 
  filter(is.na(pred2)) %>% 
  View()

train %>% 
  ggplot(aes(batAvg, pred2)) +
  geom_abline(slope = 1, intercept = 0) +
  geom_point()

names(attributes(fit2))
attr(fit2, "call")
attr(fit2, "beta")
attr(fit2, "u")
attr(fit2, "class")

train %>% 
  mutate(resid = batAvg - pred2) %>% 
  ggplot(aes(resid)) +
  geom_histogram(binwidth = 0.005)

test$pred2 <- predict(fit2, test, allow.new.levels = TRUE)
test %>% 
  ggplot(aes(batAvg, pred2)) +
  geom_abline(slope = 1, intercept = 0) +
  geom_point()
test %>% 
  mutate(resid = batAvg - pred2) %>% 
  ggplot(aes(resid)) +
  geom_histogram(binwidth = 0.005)

## For test and train sets find the 6 best and worst fitting players and plot predicted vs
## actual values

## First the worst from the training set:
trainWorst6 <- train %>% 
  group_by(playerID) %>% 
  mutate(sqError = (batAvg - pred2)^2) %>% 
  summarise(rmse = sqrt(mean(sqError, na.rm=TRUE))) %>% 
  ungroup() %>% 
  arrange(desc(rmse)) %>% 
  slice(1:6) 
train %>% 
  # filter(playerID %in% trainWorst6$playerID) %>% 
  inner_join(trainWorst6, by = "playerID") %>% 
  mutate(customLabel = paste0(nameFirst, " ", nameLast, ": RMSE = ", round(rmse, 4))) %>% 
  select(playerID, customLabel, yearID, actual = batAvg, predicted = pred2) %>% 
  gather(var, value, -c(playerID, customLabel, yearID)) %>% 
  ggplot(.) +
  geom_line(aes(x = yearID, y = value, group = var, col = var)) +
  geom_point(aes(x = yearID, y = value, group = var, col = var)) +
  facet_wrap(~customLabel) +
  scale_y_continuous(labels=scaleFUNy) +
  labs(title = "Predicted vs Actual Plots by year for worst-fitting players (Test set)",
       subtitle = "RMSE = Root Mean Squared Error",
       x = "Year Played",
       y = "Batting Average")


## Now the best from the training set:
trainBest6 <- train %>% 
  group_by(playerID) %>% 
  mutate(sqError = (batAvg - pred2)^2) %>% 
  summarise(rmse = sqrt(mean(sqError, na.rm=TRUE))) %>% 
  ungroup() %>% 
  arrange(rmse) %>% 
  slice(1:6) 
train %>% 
  # filter(playerID %in% trainWorst6$playerID) %>% 
  inner_join(trainBest6, by = "playerID") %>% 
  mutate(customLabel = paste0(nameFirst, " ", nameLast, ": RMSE = ", round(rmse, 4))) %>% 
  select(playerID, customLabel, yearID, actual = batAvg, predicted = pred2) %>% 
  gather(var, value, -c(playerID, customLabel, yearID)) %>% 
  ggplot(.) +
  geom_line(aes(x = yearID, y = value, group = var, col = var)) +
  geom_point(aes(x = yearID, y = value, group = var, col = var)) +
  facet_wrap(~customLabel) +
  scale_y_continuous(labels=scaleFUNy) +
  labs(title = "Predicted vs Actual Plots by year for best-fitting players (Train Set)",
       subtitle = "RMSE = Root Mean Squared Error",
       x = "Year Played",
       y = "Batting Average")

## What about players that havent been exposed to the model? Do the same plots for the test set.
# The default predictions for the unseen players is the fixed effects. 
# This is still better than a model that ignores random effects since it dampens the fixed
# effect coefficient estimates:
test %>% select(playerID) %>% distinct() %>% nrow()
testWorst6 <- test %>% 
  group_by(playerID) %>% 
  mutate(sqError = (batAvg - pred2)^2) %>% 
  summarise(rmse = sqrt(mean(sqError, na.rm=TRUE))) %>% 
  ungroup() %>% 
  arrange(desc(rmse)) %>% 
  slice(1:6) 
test %>% 
  # filter(playerID %in% trainWorst6$playerID) %>% 
  inner_join(testWorst6, by = "playerID") %>% 
  mutate(customLabel = paste0(nameFirst, " ", nameLast, ": RMSE = ", round(rmse, 4))) %>% 
  select(playerID, customLabel, yearID, actual = batAvg, predicted = pred2) %>% 
  gather(var, value, -c(playerID, customLabel, yearID)) %>% 
  ggplot(.) +
  geom_line(aes(x = yearID, y = value, group = var, col = var)) +
  geom_point(aes(x = yearID, y = value, group = var, col = var)) +
  facet_wrap(~customLabel) +
  scale_y_continuous(labels=scaleFUNy) +
  labs(title = "Predicted vs Actual Plots by year for worst-fitting players (Test set)",
       subtitle = "RMSE = Root Mean Squared Error",
       x = "Year Played",
       y = "Batting Average")

testBest6 <- test %>% 
  group_by(playerID) %>% 
  mutate(sqError = (batAvg - pred2)^2) %>% 
  summarise(rmse = sqrt(mean(sqError, na.rm=TRUE))) %>% 
  ungroup() %>% 
  arrange(rmse) %>% 
  slice(1:6) 
test %>% 
  # filter(playerID %in% trainWorst6$playerID) %>% 
  inner_join(testBest6, by = "playerID") %>% 
  mutate(customLabel = paste0(nameFirst, " ", nameLast, ": RMSE = ", round(rmse, 4))) %>% 
  select(playerID, customLabel, yearID, actual = batAvg, predicted = pred2) %>% 
  gather(var, value, -c(playerID, customLabel, yearID)) %>% 
  ggplot(.) +
  geom_line(aes(x = yearID, y = value, group = var, col = var)) +
  geom_point(aes(x = yearID, y = value, group = var, col = var)) +
  facet_wrap(~customLabel) +
  scale_y_continuous(labels=scaleFUNy) +
  labs(title = "Predicted vs Actual Plots by year for best-fitting players (Test set)",
       subtitle = "RMSE = Root Mean Squared Error",
       x = "Year Played",
       y = "Batting Average")

## How do we take advantage of new information? 
## For players the model hasnt seen, let's use the first two years worth of data to help
##  predict the remaining years
## To do this we'll need to predict both the
##    - BLUE: Best Linear Unbiased Estimator (for fixed effects)
##    - BLUP: Best Linear Unbiased Predictor (for random effects)

## We'll have to do this by hand