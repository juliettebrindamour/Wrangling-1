library(dslabs)
library(dplyr)
library(tidyverse)
#Combining Tables
#import murders data set 
data(murders)
head(murders)
# import US election results data
data(polls_us_election_2016)
head(results_us_election_2016)
#make a table
tab <- left_join(murders, results_us_election_2016, by = "state")
head(tab)
# make two smaller tables to demonstrate joins
tab1 <- slice(murders, 1:6) %>% select(state, population)
tab1
tab2 <- slice(results_us_election_2016, c(1:3, 5, 7:8)) %>% select(state, electoral_votes)
tab2

left_join(tab1, tab2, by = "state")

#Q5-7 intro
install.packages("Lahman")
library(Lahman)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()
Master %>% as_tibble()
#making a table of top 10 HR hitters
top_names <- top %>% left_join(Master) %>%
  select(playerID, nameFirst, nameLast, HR)
top_names

#salaries
head(Salaries)
top_salary <- Salaries %>% filter(yearID == 2016) %>%
  right_join(top_names) %>%
  select(nameFirst, nameLast, teamID, HR, salary)
top_salary

#awards
head(AwardsPlayers)
AwardsPlayers <- AwardsPlayers %>% filter(yearID == 2016)
AwardsPlayers
intersect(AwardsPlayers$playerID, top_names$playerID)
