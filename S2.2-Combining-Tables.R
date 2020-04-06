library(tidyverse)
library(dslabs)
library(dplyr)

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
#the states contained in these two tables differ
#experimenting w/ join functions. Join functions join tables by variable and try to make them match
left <- left_join(tab1, tab2) #joining by states in tab 1 (NAs coming from tab2)
left 
left <- tab1 %>% left_join(tab2) #same thing
left
right <- tab1 %>% right_join(tab2) #joining by states in tab2 (NAs coming from tab1)
right
inner <- inner_join(tab1, tab2) #to keep only rows that have information in both tables
inner
full <- full_join(tab1, tab2) #to keep all rows and fill in missing parts with NAs
full
semi <- semi_join(tab1, tab2) #lets us keep the part of the first table for which we have info
#in the second table
semi
anti <- anti_join(tab1, tab2) #keeps the elements of the first table for which there is no info
#in the second
anti

#Binding 
#Unlike join functions, bind functions disregard variables/ row order.
#bind_cols() binds two columns together to make a tibble. Can also bind data frames
#bind_rows() binds rows instead of columns

#Set operators
#intersect() takes the intersect of vectors or data frames.
#For data frames: takes the intersections of rows for tables having the same column names.
intersect(1:10, 6:15)
intersect(c("a","b","c"), c("b","c","d"))
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
intersect(tab1, tab2)

#Union works on vectors but also on data frames having the same column names.
#you get the rows they have in common + the other rows called
union(1:10, 6:15)
union(c("a","b","c"), c("b","c","d"))
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
union(tab1, tab2)

# setdiff() takes the difference between 2 data frames (ex. what is in tab 1 but not tab2)
#it is not symmetric, order of args matters
setdiff(1:10, 6:15)
setdiff(6:15, 1:10)
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
setdiff(tab1, tab2)

# setequal determines whether sets have the same elements, regardless of order
setequal(1:5, 1:6)
setequal(1:5, 5:1)
setequal(tab1, tab2)

