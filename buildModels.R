library(plyr)
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
# year(data$debutDate[1])
# year(data$debutDate)

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

# The software complains and estimates are very small for the squared random effects. 
## Let's remove them and do a simpler model.
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
# train %>% 
  # select(playerID, nameFirst, nameLast, GS, estSalary, cumAwards, soPer, yearService, batAvg, pred1) %>%   filter(is.na(pred1)) %>% View()

# Looks like there's lots of missin salaries in the data. try again without it
# also awards are very rare. remove for now.
fit2 <- lmer(batAvg ~ 1 
             + GS 
             # + estSalary 
             # + cumAwards 
             + soPer
             + yearService
             + (1 + yearService | playerID), 
             data = train)
summary(fit2)
head(lme4::ranef(fit2)[[1]])
lme4::fixef(fit2)

train$pred2 <- predict(fit2, train, re.form = NULL) # includes all random effects
# train %>% 
#   select(yearID, playerID, nameFirst, nameLast, GS, estSalary, cumAwards, soPer, yearService, batAvg, pred2) %>% 
#   filter(is.na(pred2)) %>% 
#   View()

train %>% 
  ggplot(aes(batAvg, pred2)) +
  geom_abline(slope = 1, intercept = 0) +
  geom_point()

names(attributes(fit2))
# attr(fit2, "call")
# attr(fit2, "beta")
# attr(fit2, "u")
# attr(fit2, "class")

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
plotTrainWorst <- train %>% 
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
  labs(title = "Predicted vs Actual Plots by year for worst-fitting players (Train set)",
       subtitle = "RMSE = Root Mean Squared Error",
       x = "Year Played",
       y = "Batting Average")
plotTrainWorst
ggsave("plotTrainWorst.png", plot = plotTrainWorst, path = here::here('plots'))

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
plotTestWorst <- test %>% 
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
plotTestWorst
ggsave("plotTestWorst.png", plot = plotTestWorst, path = here::here('plots'))

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
## For players the model hasnt seen, let's use the first three years worth of data to help
##  predict the remaining years
## To do this we'll need to predict both the
##    - BLUE: Best Linear Unbiased Estimator (for fixed effects)
##    - BLUP: Best Linear Unbiased Predictor (for random effects)

## We'll have to do this by hand 
## Recall $y = X\beta + Z\gamma + \epsilon$ and
## Var(y) = V = ZGZ' + R where Var(\gamma) = G and Var(\epsilon) = R
## Can be shown (see https://support.sas.com/documentation/cdl/en/statug/63033/HTML/default/viewer.htm#statug_mixed_sect022.htm) that
## \hat{\beta} = (X'\hat{V}^{-1}X)^-X'\hat{V}^{-1}y and
## \hat{\gamma} = \hat{G}Z'\hat{V}^{-1}(y - X\hat{\beta})

# v <- VarCorr(fit2); v
# summary(fit2)
# names(attributes(v))
# G <- as.matrix(Matrix::bdiag(v)); G
# sigma(fit2)
# model.matrix(fit2)

## take 6 worst players again: calculate \hat{\beta} and \hat{\gamma} for their first 3 years
## then use that info to predict the new few years of their careers
first3Years <- test %>% 
  # filter(playerID %in% trainWorst6$playerID) %>% 
  inner_join(testBest6, by = "playerID") %>% 
  group_by(playerID) %>% 
  mutate(rowNum = row_number()) %>% 
  ungroup() %>% 
  filter(rowNum <= 3) 
## how many years did each play?
careerLengths <- test %>% 
  filter(playerID %in% first3Years$playerID) %>% 
  group_by(playerID) %>% 
  summarise(maxYearService = max(yearService)) %>% 
  ungroup() 
careerLengths

# merge first 3 years with career lengths to know how long loops should be
first3 <- first3Years %>% left_join(careerLengths, by = "playerID")



# estYearlyBA <- function(data, object, ID, yearsToPred){
#   data = first3Years; object = fit2; ID = "andruel01"; yearsToPred = 10
#   v <- VarCorr(object);
#   G <- as.matrix(Matrix::bdiag(v))
#   redData <- data %>% filter(playerID == ID)
#   Z = matrix(cbind(rep(1, yearsToPred), 1:yearsToPred), ncol = 2)
#   # Var(y) = V = ZGZ' + R where Var(\gamma) = G and Var(\epsilon) = R
#   V = Z%*%G%*%t(Z) + sigma(fit2)*diag(yearsToPred)
#   ## \hat{\beta} = (X'\hat{V}^{-1}X)^-X'\hat{V}^{-1}y 
#   X = data %>% 
#     filter(playerID == ID) %>% 
#     mutate(ones = 1) %>% 
#     select(ones, GS, soPer, yearService) %>% 
#     as.matrix()
#   rows <- as.numeric(data %>% filter(playerID == ID) %>% nrow())
#   Z_red = matrix(cbind(rep(1, rows), 1:rows), ncol = 2)
#   V_red = Z_red%*%G%*%t(Z_red) + sigma(fit2)*diag(rows)
#   y_red = data %>% filter(playerID == ID) %>% select(batAvg)
#   # beta_hat = solve(t(X)%*%solve(V_red)%*%X)%*%t(X)%*%solve(V_red)%*%y_red$batAvg
#   beta_hat <- fixef(fit2)
#   ## \hat{\gamma} = \hat{G}Z'\hat{V}^{-1}(y - X\hat{\beta})
#   gamma_hat = G%*%t(Z_red)%*%solve(V_red)%*%(y_red$batAvg - X%*%beta_hat)
#   preds = X%*%beta_hat + Z%*%gamma_hat
#   return(preds)
# }
# estYearlyBA(data = first3Years, object = fit2, ID = "andruel01", yearsToPred = 10)

## Build custom BLUP on Test Set subjects
allSubPredParMC <- function(obj, data, y, cores){
  require('lme4')
  require('parallel')
  require('plyr')
  
  # obj: lme4 object (merMod)
  # data: data.frame
  # y: character
  # cores: integer (use detectCores to figure out what is available)
  # obj = fit2;data = first3Years;y = "batAvg";cores = 4
  # obj = fit3;data = first3_simple;y = "batAvg";cores = 4
  
  getSubPred <- function(obj, playerID, data, y){
    # obj = fit2; playerID = "andruel01"; data = first3Years; y = "batAvg"
    # playerID = ids[1]
    D <- matrix(c(VarCorr(obj))$playerID[1:2, 1:2], 2, 2) # change 2 to number of random effects
    len <- sum(ifelse(data$playerID==playerID, 1, 0))
    onlyplayerID <- data[which(data$playerID==playerID),]
    Zt <- matrix(data=rbind(c(rep(1, len))
                            , seq(min(onlyplayerID$yearService)
                                  , max(onlyplayerID$yearService), 1))
                 , 2, len)
    Z <- t(Zt)
    Sigma <- diag(attr(VarCorr(obj), "sc")^2, len, len)
    onlyplayerID$predPop <- (fixef(obj))[names(fixef(obj)) %in% c("(Intercept)")] + (fixef(obj))[names(fixef(obj)) %in% c("GS")] + (fixef(obj))[names(fixef(obj)) %in% c("soPer")] + (fixef(obj))[names(fixef(obj)) %in% c("yearService")]
    uhat <- D%*%Zt%*%solve(Z%*%D%*%Zt + Sigma)%*%as.matrix(onlyplayerID[,names(onlyplayerID) %in% y] - onlyplayerID$predPop)    
    onlyplayerID$predSubjectCustom <- as.numeric(onlyplayerID$predPop + Z%*%uhat)
    onlyplayerID$ranIntercept <- as.numeric(uhat[1])
    onlyplayerID$ranSlope <- as.numeric(uhat[2])
    return(onlyplayerID)
  }
  ids <- unique(data$playerID)
  # temp <- ldplyr(mclapply(X=ids, FUN=getSubPred, obj=obj, data=data, y=y, mc.cores=cores), data.frame)
  temp <- plyr::ldply(mclapply(X=ids, FUN=getSubPred, obj=obj, data=data, y=y, mc.cores=cores), data.frame)
  
  # Also return random effects parameters to returned data frame
  # note: if taken from never-before seen players, will return Nulls
  ranefs <- ranef(obj)[[1]]
  names(ranefs) <- c("ranInterceptTrue", "ranSlopeTrue")
  ranefs$playerID <- rownames(ranefs); rownames(ranefs) <- c(1:nrow(ranefs))
  require('dplyr')
  temp2 <- dplyr::left_join(temp, ranefs, by="playerID")
  return(temp2)
}
first3_blups <- allSubPredParMC(obj = fit2,data = first3,y = "batAvg",cores = 4) 

## Demonstrate power of blup ####
# First refit model with only time and blups
fit3 <- lmer(batAvg ~ 1
             + yearService
             + (1 + yearService | playerID), 
             data = train)
summary(fit3)
head(lme4::ranef(fit3)[[1]])
lme4::fixef(fit3)

train$pred3 <- predict(fit3, train, re.form = NULL)
test$pred3 <- predict(fit3, test, allow.new.levels = TRUE)
testWorst6_3 <- test %>% 
  group_by(playerID) %>% 
  mutate(sqError = (batAvg - pred3)^2) %>% 
  summarise(rmse = sqrt(mean(sqError, na.rm=TRUE))) %>% 
  ungroup() %>% 
  arrange(desc(rmse)) %>% 
  slice(1:6) 
plotTestWorst3 <- test %>% 
  # filter(playerID %in% trainWorst6$playerID) %>% 
  inner_join(testWorst6_3, by = "playerID") %>% 
  mutate(customLabel = paste0(nameFirst, " ", nameLast, ": RMSE = ", round(rmse, 4))) %>% 
  select(playerID, customLabel, yearID, actual = batAvg, predicted = pred3) %>% 
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
plotTestWorst3
ggsave("plotTestWorst3.png", plot = plotTestWorst3, path = here::here('plots'))

# testBest6_3 <- test %>% 
#   group_by(playerID) %>% 
#   mutate(sqError = (batAvg - pred3)^2) %>% 
#   summarise(rmse = sqrt(mean(sqError, na.rm=TRUE))) %>% 
#   ungroup() %>% 
#   arrange(rmse) %>% 
#   slice(1:6) 
# test %>% 
#   # filter(playerID %in% trainWorst6$playerID) %>% 
#   inner_join(testBest6_3, by = "playerID") %>% 
#   mutate(customLabel = paste0(nameFirst, " ", nameLast, ": RMSE = ", round(rmse, 4))) %>% 
#   select(playerID, customLabel, yearID, actual = batAvg, predicted = pred3) %>% 
#   gather(var, value, -c(playerID, customLabel, yearID)) %>% 
#   ggplot(.) +
#   geom_line(aes(x = yearID, y = value, group = var, col = var)) +
#   geom_point(aes(x = yearID, y = value, group = var, col = var)) +
#   facet_wrap(~customLabel) +
#   scale_y_continuous(labels=scaleFUNy) +
#   labs(title = "Predicted vs Actual Plots by year for best-fitting players (Test set)",
#        subtitle = "RMSE = Root Mean Squared Error",
#        x = "Year Played",
#        y = "Batting Average")

## These results are very rudimentary. 
## Let's recalculate the blup for each day in the future to get better estimates
# Demonstrate power of blup

## Once again start by calculating the blups for the first 3 years of the 6 worst players (in terms of their RMSE)
first3_simple <- test %>% 
  # filter(playerID %in% trainWorst6$playerID) %>% 
  inner_join(testWorst6_3, by = "playerID") %>% 
  group_by(playerID) %>% 
  mutate(rowNum = row_number()) %>% 
  ungroup() %>% 
  filter(rowNum <= 3) 

## build random effects parameters on this data set:
# first3_blups <- allSubPredParMC(obj = fit2,data = first3,y = "batAvg",cores = 4) 
allSubPredParMC3 <- function(obj, data, y, cores){
  require('lme4')
  require('parallel')
  require('plyr')
  
  # obj: lme4 object (merMod)
  # data: data.frame
  # y: character
  # cores: integer (use detectCores to figure out what is available)
  # obj = fit2;data = first3Years;y = "batAvg";cores = 4
  # obj = fit3;data = first3_simple;y = "batAvg";cores = 4
  
  getSubPred <- function(obj, playerID, data, y){
    # obj = fit2; playerID = "andruel01"; data = first3Years; y = "batAvg"
    # playerID = ids[1]
    D <- matrix(c(VarCorr(obj))$playerID[1:2, 1:2], 2, 2) # change 2 to number of random effects
    len <- sum(ifelse(data$playerID==playerID, 1, 0))
    onlyplayerID <- data[which(data$playerID==playerID),]
    Zt <- matrix(data=rbind(c(rep(1, len))
                            , seq(min(onlyplayerID$yearService)
                                  , max(onlyplayerID$yearService), 1))
                 , 2, len)
    Z <- t(Zt)
    Sigma <- diag(attr(VarCorr(obj), "sc")^2, len, len)
    onlyplayerID$predPop <- (fixef(obj))[names(fixef(obj)) %in% c("(Intercept)")] +  (fixef(obj))[names(fixef(obj)) %in% c("yearService")]
    uhat <- D%*%Zt%*%solve(Z%*%D%*%Zt + Sigma)%*%as.matrix(onlyplayerID[,names(onlyplayerID) %in% y] - onlyplayerID$predPop)    
    onlyplayerID$predSubjectCustom <- as.numeric(onlyplayerID$predPop + Z%*%uhat)
    onlyplayerID$ranIntercept <- as.numeric(uhat[1])
    onlyplayerID$ranSlope <- as.numeric(uhat[2])
    return(onlyplayerID)
  }
  ids <- unique(data$playerID)
  # temp <- ldplyr(mclapply(X=ids, FUN=getSubPred, obj=obj, data=data, y=y, mc.cores=cores), data.frame)
  temp <- plyr::ldply(mclapply(X=ids, FUN=getSubPred, obj=obj, data=data, y=y, mc.cores=cores), data.frame)
  
  # Also return random effects parameters to returned data frame
  # note: if taken from never-before seen players, will return Nulls
  ranefs <- ranef(obj)[[1]]
  names(ranefs) <- c("ranInterceptTrue", "ranSlopeTrue")
  ranefs$playerID <- rownames(ranefs); rownames(ranefs) <- c(1:nrow(ranefs))
  require('dplyr')
  temp2 <- dplyr::left_join(temp, ranefs, by="playerID")
  
  ## add fixed slope and intercept
  fixefs <- fixef(obj)
  # names(fixefs) <- c("fixedIntercept", "fixedSlope")
  temp2 %>% 
    mutate(fixedIntercept = fixefs[1],
           fixedSlope = fixefs[2])
  # return(temp2)
}
first3_simple_blups <- allSubPredParMC3(obj = fit3,data = first3_simple,y = "batAvg",cores = 4) 

## Now we predict the next 5 years by increasing the yearService by 1 and recalculating the blup each iteration
updateWithBlup <- function(id, data, obj, y, years){
  # id = playerID from dataset
  # data = name of data set created from allSubPredParMC3 function
  # obj = object created from lme4::lmer
  # y = response name
  # years = number of predictions into future needed
  ## test
  # id = "escobal02"; data = first3_simple_blups; obj = fit3; y = "batAvg"; years = 5
  library(dplyr)
  library(lme4)
  datared <- data %>% 
    filter(playerID==id) %>% 
    select(playerID, yearID, yearService, pred3, one_of(y), predPop, predSubjectCustom, fixedIntercept, ranIntercept, fixedSlope, ranSlope) %>% 
    mutate(history = TRUE)
  firstRowCalc <- nrow(datared)
  for(i in 1:years){
    
    Zt = matrix(data=rbind(c(rep(1, max(datared$yearService)+1)), 
                          seq(1, max(datared$yearService)+1, 1)
                          )
               , 2, max(datared$yearService)+1)
    Z = t(Zt)
    D <- matrix(c(VarCorr(obj))$playerID[1:2, 1:2], 2, 2)
    Sigma <- diag(attr(VarCorr(obj), "sc")^2, max(datared$yearService)+1, max(datared$yearService)+1)
    X = Z
    V = Z%*%D%*%t(Z) + Sigma
    
    newRow <- datared %>% slice(nrow(.))
    newRow$yearID = newRow$yearID + 1
    newRow$yearService = newRow$yearService + 1
    newRow$batAvg = newRow$fixedIntercept + newRow$ranIntercept + (newRow$fixedSlope + newRow$ranSlope)*newRow$yearService
    beta = solve(t(X)%*%solve(V)%*%X)%*%t(X)%*%solve(V)%*%as.matrix(c(datared$batAvg, newRow$batAvg))
    newRow$fixedIntercept <- beta[1,1]
    newRow$fixedSlope <- beta[2,1]
    newRow$predPop = newRow$fixedIntercept + newRow$fixedSlope*newRow$yearService
    newRow$history = FALSE
    datared <- rbind(datared, newRow)
    
    uhat = D%*%Zt%*%solve(Z%*%D%*%Zt + Sigma)%*%as.matrix(datared$batAvg - datared$predPop)
    datared$ranIntercept[i + firstRowCalc] <- uhat[1,1]
    datared$ranSlope[i + firstRowCalc] <- uhat[2,1]
    datared$predSubjectCustom[i + firstRowCalc] = datared$predPop[i + firstRowCalc] + uhat[1,1] + uhat[2,1]*datared$yearService[i + firstRowCalc]
    cat("i = ", i, ".\n",sep = "")
    print(datared)
    
    
  }
  return(datared)
}
escobal02_preds <- updateWithBlup(id = "escobal02", data = first3_simple_blups, obj = fit3, y = "batAvg", years = 5)
## plot preds vs actuals for this player
escobal02_3 <- test %>%
  inner_join(testWorst6_3, by = "playerID") %>%
  filter(playerID %in% c("escobal02")) %>% 
  left_join(escobal02_preds %>% filter(yearService>3) %>% select(newBatAvg = batAvg, everything()), by = c("yearID", "playerID", "yearService")) %>% 
  mutate(customLabel = paste0(nameFirst, " ", nameLast, ": RMSE = ", round(rmse, 4))) %>% 
  select(playerID, customLabel, yearID, actual = batAvg, predictedFixed = pred3.x, predictedBLUP = newBatAvg) %>% 
  gather(var, value, -c(playerID, customLabel, yearID)) %>% 
  ggplot(.) +
  geom_line(aes(x = yearID, y = value, group = var, col = var)) +
  geom_point(aes(x = yearID, y = value, group = var, col = var)) +
  facet_wrap(~customLabel) +
  scale_x_continuous(labels=scaleFUN) +
  scale_y_continuous(labels=scaleFUNy) +
  labs(title = "Predicted vs Actual Plots by year for worst-fitting players (Test set)",
       subtitle = "First 3 years used for Green Prediction",
       x = "Year Played",
       y = "Batting Average")
escobal02_3
ggsave(filename = "escobal02_3.png",plot = escobal02_3,path = here::here('plots'))
