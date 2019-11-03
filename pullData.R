library(Lahman)
library(tidyverse)
library(lubridate)
library(here)

data(People)
# View(People)
str(People)

## Select all players who debuted no earlier than 1999 ####
## The selection of 1999 is arbitrary, as anyone could argue what constitutes
## an 'era' of the game.
## see https://www.billjamesonline.com/dividing_baseball_history_into_eras/
nrow(People)
peopleDebut1999 <- People %>% 
  mutate(debutDate = as.Date(debut, "%Y-%m-%d")) %>% 
  filter(lubridate::year(debutDate) >= 1999) %>% 
  select(playerID, birthYear, birthMonth, birthDay, nameFirst, nameLast, debutDate)

## Merge people with batting stats, remove pitchers ####
data(Batting)
str(Batting)
data(Pitching)
peopleDebut1999 %>% 
  inner_join(Batting, by = "playerID") %>% ## join batting stats
  anti_join(Pitching, by = "playerID") %>% ## remove pitchers
  arrange(playerID, yearID) %>% 
  View()
