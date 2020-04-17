library(dslabs)
library(tidyverse)
library(dplyr)
library(readr)

#3.1: Section 3 part 1
#STRING PARSING
# read in raw murders data from Wikipedia (to run this code, put URL back on same line)
url <- "https://en.wikipedia.org/w/index.php?title=Gun_violence_in_the_United_States
_by_state&direction=prev&oldid=810166167"
murders_raw <- read_html(url) %>% 
  html_nodes("table") %>% 
  html_table() %>%
  .[[1]] %>%
  setNames(c("state", "population", "total", "murder_rate"))
# inspect data and column classes
head(murders_raw)
class(murders_raw$population)
class(murders_raw$total)
#converting numeric data contained in character strings into numeric representations required
#to make plots, summarize data or fit models in R
#parse_number() does this (ex. removing commas from large numbers)

#Defining Strings: single and double quotes and how to escape
s <- "Hello!"    # double quotes define a string
s <- 'Hello!'    # single quotes define a string
s <- `Hello`    # backquotes do not

#s <- "10""    # error - unclosed quotes
s <- '10"'    # correct

# cat() shows what the string actually looks like inside R
cat(s)

s <- "5'"
cat(s)

# to include both single and double quotes in string, escape the quotes with \
#(avoids an unclosed quote)
#s <- '5'10"'    # error
#s <- "5'10""    # error
s <- '5\'10"'    # correct
cat(s)
s <- "5'10\""    # correct
cat(s)

#Case study 1: Murders in the US
#checking for data file
head(murders_raw) 
#detect if the columns have commas (creating functions) using str_detect()
commas <- function(x) {any(str_detect(x, ","))}
murders_raw %>% summarize_all(funs(commas)) 
#use str_replace_all() to remove the commas and then convert to numeric
test_1 <- str_replace_all(murders_raw$population, ",", "")
test_1 <- as.numeric(test_1)
#other way to do it
test_2 <- parse_number(murders_raw$population)
identical(test_1, test_2) #returns TRUE
#doing it to columns 2 and 3
murders_new <- murders_raw %>% mutate_at(2:3, parse_number)
murders_new %>% head
