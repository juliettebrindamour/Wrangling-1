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

#Case Study 2: Reported Heights
# load raw heights data and inspect
data(reported_heights)
class(reported_heights$height) #returns character bc some people answered the survey using the
#incorrect format (not height in inches)

# convert to numeric, inspect, count NAs. Gives too many NAs
x <- as.numeric(reported_heights$height)
head(x)
sum(is.na(x))

# keep only entries that result in NAs in order to inspect them and find patterns.
reported_heights %>% mutate(new_height = as.numeric(height)) %>%
  filter(is.na(new_height)) %>% 
  head(n=10)

# calculate cutoffs that cover 99.999% of human population (in order to exclude unrealisticc
#entries)
alpha <- 1/10^6
qnorm(1-alpha/2, 69.1, 2.9)
qnorm(alpha/2, 63.7, 2.7)

# keep only entries that either result in NAs or are outside the plausible range of heights
#we will view these later and try to figure out how to convert everything to inches
not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}

# extracting number of problematic entries from reported_heights dataset
problems <- reported_heights %>% 
  filter(not_inches(height)) %>%
  .$height
length(problems)

# 10 examples of x'y or x'y" or x'y\"
pattern <- "^\\d\\s*'\\s*\\d{1,2}\\.*\\d*'*\"*$"
str_subset(problems, pattern) %>% head(n=10) %>% cat

# 10 examples of x.y or x,y
pattern <- "^[4-6]\\s*[\\.|,]\\s*([0-9]|10|11)$"
str_subset(problems, pattern) %>% head(n=10) %>% cat

# 10 examples of entries in cm rather than inches
ind <- which(between(suppressWarnings(as.numeric(problems))/2.54, 54, 81) ) #(reported height
# divided by 2.54 to convert to inches, between 54 and 81)
ind <- ind[!is.na(ind)]
problems[ind] %>% head(n=10) %>% cat

#RegEx (Regular Expressions)
#str_detect() detects a pattern in a string
# detect whether a comma is present
pattern <- ","
str_detect(murders_raw$total, pattern) 

#str_subset() (in stringr, from tidyverse) function pulls out all the entries that 
#contain a specific pattern
# show the subset of strings including "cm"
str_subset(reported_heights$height, "cm")

# use the "or" symbol: "|" (bar) inside a regex
#(another useful symbol is "\\d" which means any didgit up to 9)
yes <- c("180 cm", "70 inches")
no <- c("180", "70''")
s <- c(yes, no)
str_detect(s, "cm") | str_detect(s, "inches")
str_detect(s, "cm|inches")

# str_view() highlight the first occurrence of a pattern
str_view(s, pattern)

# str_view_all() highlight all instances of a pattern
str_view_all(s, pattern)

#Character Classes, Anchors and Quantifiers
yes <- c("5", "6", "5'10", "5 feet", "4'11")
no <- c("", ".", "Five", "six")
s <- c(yes, no)
pattern <- "\\d"

# [56] means 5 or 6
str_view(s, "[56]")

# [4-7] means 4, 5, 6 or 7
yes <- as.character(4:7)
no <- as.character(1:3)
s <- c(yes, no)
str_detect(s, "[4-7]")

# ^ means start of string, $ means end of string
pattern <- "^\\d$"
yes <- c("1", "5", "9")
no <- c("12", "123", " 1", "a4", "b")
s <- c(yes, no)
str_view(s, pattern)

# curly braces define quantifiers: 1 or 2 digits 
pattern <- "^\\d{1,2}$"
yes <- c("1", "5", "9", "12")
no <- c("123", "a4", "b")
str_view(c(yes, no), pattern)

# combining character class, anchors and quantifier
pattern <- "^[4-7]'\\d{1,2}\"$"
yes <- c("5'7\"", "6'2\"",  "5'12\"")
no <- c("6,2\"", "6.2\"","I am 5'11\"", "3'2\"", "64")
str_detect(yes, pattern)
str_detect(no, pattern)
