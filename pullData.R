library(Lahman)
library(tidyverse)
library(lubridate)
library(here)

data(People)
# View(People)
str(People)

## Select all players who debuted no earlier than 1993 ####
## The selection of debut year is arbitrary, as anyone could argue what 
## constitutes an 'era' of the game.
## see https://www.billjamesonline.com/dividing_baseball_history_into_eras/
nrow(People)
peopleDebut1993 <- People %>% 
  mutate(debutDate = as.Date(debut, "%Y-%m-%d")) %>% 
  filter(lubridate::year(debutDate) >= 1993) %>% 
  select(playerID, birthYear, birthMonth, birthDay, nameFirst, nameLast, debutDate)

# prep awards data for joining. 
# joining as is will duplicate rows for players who won more than 1 award per year
awardsWide <- AwardsPlayers %>% 
  filter(yearID >= 1993) %>% 
  mutate(ones = 1) %>% 
  # select(awardID) %>% distinct() %>% 
  select(playerID, yearID:notes, awardID, ones) %>% 
  spread(awardID, ones) %>% 
  mutate_all(~replace(., is.na(.), 0)) 

# filter out starters, players who avg over 81 starts per year
Appearances %>% 
  ggplot(., aes(GS)) +
  geom_histogram()
starters <- Appearances %>% 
  filter(yearID >= 1993) %>% 
  group_by(playerID) %>% 
  summarise(avgGS = mean(GS, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(desc(avgGS)) %>% 
  filter(avgGS >= 81)
appStarters <- Appearances %>% inner_join(starters, by = "playerID")

## Merge people with batting stats, remove pitchers ####
data(Batting)
str(Batting)
data(Pitching)
data(Appearances)
data(AwardsPlayers)
data(Salaries)
data <- peopleDebut1993 %>% 
  inner_join(Batting, by = "playerID") %>% ## join batting stats
  anti_join(Pitching, by = "playerID") %>% ## remove pitchers
  inner_join(appStarters, by = c("playerID", "yearID", "lgID", "teamID")) %>% # nrow() # 12648
  left_join(awardsWide, by = c("playerID", "yearID", "lgID")) %>% # nrow() # 4041
  # filter(!is.na(awardID)) %>% View()
  left_join(Salaries, by = c("playerID", "yearID", "lgID", "teamID")) %>% 
  arrange(playerID, yearID) %>% 
  group_by(playerID) %>% 
  mutate(yearService = row_number(),
         birthDate = as.Date(paste0(birthYear, "-", birthMonth, "-", birthDay), "%Y-%m-%d"),
         yearsSinceDebut = yearID - min(yearID),
         age = lubridate::time_length(difftime(debutDate, birthDate), "years") + yearsSinceDebut,
         cumGamesPlayed = cumsum(G),
         batAvg = H/AB,
         careerBatAvg = cumsum(H)/cumsum(AB),
         obp = (H + BB + IBB + HBP)/(AB + BB + IBB + HBP + SF),
         slug = (H + X2B + (X3B*2) + (HR*3))/AB,
         ops = obp + slug,
         soPer = SO/(AB-H),
         awards = `ALCS MVP` + `All-Star Game MVP` + `Babe Ruth Award` + `Branch Rickey Award` + `Comeback Player of the Year` + `Gold Glove` +  `Hank Aaron Award` + `Hutch Award` + `Lou Gehrig Memorial Award` + `Most Valuable Player` + `NLCS MVP` + `Outstanding DH Award` + `Roberto Clemente Award` + `Rookie of the Year` + `Silver Slugger` + `Triple Crown` + `TSN All-Star` + `TSN Fireman of the Year` + `TSN Major League Player of the Year` + `World Series MVP`,
         cumAwards = cumsum(awards)
         ) %>% 
  ungroup() %>% 
  mutate_at(vars(tie:`World Series MVP`, awards, cumAwards), ~replace(., is.na(.), 0))

