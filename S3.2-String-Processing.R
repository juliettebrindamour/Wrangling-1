library(tidyverse)
library(dslabs)
library(dplyr)
library(readr)
install.packages("htmlwidgets")
library(htmlwidgets)

#CASE STUDY 2: REPORTED HEIGHTS
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

#RegEx (REGULAR EXPRESSIONS)
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

#CHARACTER CLASSES, ANCHORS AND QUANTIFIERS
#defining patterns we know should match those we are testing (yes) and ones that shouldn't (no)
#for the purpose of learning (does nto belong to the dataset).
yes <- c("5", "6", "5'10", "5 feet", "4'11")
no <- c("", ".", "Five", "six")
s <- c(yes, no)
pattern <- "\\d"
#Character Classes defined with square brackets 
#[56] means 5 or 6
str_view(s, "[56]")
# [4-7] means 4, 5, 6 or 7
# in regex everythign is a character, so we need to use as.character(). But the characters
#follow a numeric order
yes <- as.character(4:7)
no <- as.character(1:3)
s <- c(yes, no)
str_detect(s, "[4-7]")
#Anchors let us define patterns that must start or end at specific places
# ^ means start of string, $ means end of string
pattern <- "^\\d$" #read as "start of string followed by one didgit followed by end of string"
yes <- c("1", "5", "9")
no <- c("12", "123", " 1", "a4", "b")
s <- c(yes, no)
str_view(s, pattern)
#Quantifiers are used to say how many times a character can be found / repeated
# curly braces define quantifiers: 1 or 2 digits
#for example, inches can have one or two didgits
pattern <- "^\\d{1,2}$"
yes <- c("1", "5", "9", "12")
no <- c("123", "a4", "b")
str_view(c(yes, no), pattern)
# combining character class, anchors and quantifier
pattern <- "^[4-7]'\\d{1,2}\"$"
#reads: "start of string, one didgit between 4 and 7, feet symbol, one or two didgits,
#inches symbol (\"), end of string"
yes <- c("5'7\"", "6'2\"",  "5'12\"")
no <- c("6,2\"", "6.2\"","I am 5'11\"", "3'2\"", "64")
str_detect(yes, pattern)
str_detect(no, pattern)

#SEARCH AND REPLACE WITH REGEX
# number of entries matching our desired pattern
pattern <- "^[4-7]'\\d{1,2}\"$"
sum(str_detect(problems, pattern))
# inspect examples of entries with problems (inches written out or 2x single quotes)
problems[c(2, 10, 11, 12, 15)] %>% str_view(pattern)
str_subset(problems, "inches")
str_subset(problems, "''")

# replace or remove feet/inches words before matching (we removed inches symbol from pattern)
# str_replace() replaces one thing with another
pattern <- "^[4-7]'\\d{1,2}$"
problems %>% 
  str_replace("feet|ft|foot", "'") %>% # replace feet, ft, foot with ' 
  str_replace("inches|in|''|\"", "") %>% # remove all inches symbols
  str_detect(pattern) %>% 
  sum()

# R does not ignore whitespace, \\s represents whitespace
# Looking for entries with whitespace
pattern_2 <- "^[4-7]'\\s\\d{1,2}\"$"
str_subset(problems, pattern_2)

#We want to update our pattern to include spaces but not require them:
# * means 0 or more instances of the previous character
yes <- c("AB", "A1B", "A11B", "A111B", "A1111B")
no <- c("A2B", "A21B")
str_detect(yes, "A1*B")
str_detect(no, "A1*B")
# test how *, ?(none or once) and +(once or more) differ
data.frame(string = c("AB", "A1B", "A11B", "A111B", "A1111B"),
           none_or_more = str_detect(yes, "A1*B"),
           nore_or_once = str_detect(yes, "A1?B"),
           once_or_more = str_detect(yes, "A1+B"))
# update pattern by adding optional spaces before and after feet symbol
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
problems %>% 
  str_replace("feet|ft|foot", "'") %>% # replace feet, ft, foot with ' 
  str_replace("inches|in|''|\"", "") %>% # remove all inches symbols
  str_detect(pattern) %>% 
  sum()

#GROUPS W/ REGEX
#trying to fix entries like x.y, x,y and x y to format x'y
# define regex with and without groups
pattern_without_groups <- "^[4-7],\\d*$"
pattern_with_groups <-  "^([4-7]),(\\d*)$"
# create examples
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
# demonstrate that groups dont affect the search
str_detect(s, pattern_without_groups)
str_detect(s, pattern_with_groups)
# demonstrate difference between str_match and str_extract
#str_match() extracts the values of the groupes we defined
#str_extract() extracts only strings that match a pattern, not values defined by groups
str_match(s, pattern_with_groups)
str_extract(s, pattern_with_groups)

# improve the pattern to recognize more events
# \\i means i-th group
#replace comma with ' only if it is between two didgits (between group 1 and 2)
pattern_with_groups <-  "^([4-7]),(\\d*)$"
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
str_replace(s, pattern_with_groups, "\\1'\\2")

# final pattern
#reads: start of string, gr.1(one didgit between 4 and 7), none or more white spaces, 
#[feet symbol is either , . or at least one space], none or more white spaces, gr.2(none or more
#didgits), end of string.
pattern_with_groups <-"^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"

# combine stringr commands with the pipe
str_subset(problems, pattern_with_groups) %>% head
str_subset(problems, pattern_with_groups) %>% 
  str_replace(pattern_with_groups, "\\1'\\2") %>% head #almost works except one entry w/ 25"

#TESTING AND IMPROVING
# function to detect entries with problems
not_inches_or_cm <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- !is.na(inches) &
    ((inches >= smallest & inches <= tallest) |
       (inches/2.54 >= smallest & inches/2.54 <= tallest))
  !ind
}
# identify entries with problems
problems <- reported_heights %>% 
  filter(not_inches_or_cm(height)) %>%
  .$height
length(problems)

converted <- problems %>% 
  str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
  str_replace("inches|in|''|\"", "") %>%  #remove inches symbols
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") #change format to gr.1'gr.2"

# find proportion of entries that fit the pattern after reformatting
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
mean(index)

converted[!index]    # show problems
