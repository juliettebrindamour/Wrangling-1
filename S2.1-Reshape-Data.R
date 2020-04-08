library(tidyverse)
library(dslabs)
library(readxl)
library(dplyr)
#Tidy Data
#Getting to raw data (in wide data format) and saving it, wide = each row has several obs 
#and one variable stored in header
path <- system.file("extdata", package = "dslabs")
list.files(path)
filename <- file.path(path, "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)
filename <- "fertility-two-countries-example.csv"
fullpath <- file.path(path, filename)
fullpath
file.copy(fullpath, getwd())
#we want data from 1960 to 2015
select(wide_data, '1960':'2015')

#Reshaping Data
#gather() function: 
#3rd arg specifies columns that will be gathered
#1st arg sets name of column that is currently kept in the wide data column name
#2nd arg sets name of column that holds values in column cells
new_tidy_data <- wide_data %>% 
  gather(year, fertility, '1960':'2015')
head(new_tidy_data)
class(new_tidy_data$year)
#the year column is a character but we want it to be an interger 
#gather function has a convert argument
new_tidy_data <- wide_data %>% 
  gather(year, fertility, '1960':'2015', convert = TRUE)
class(new_tidy_data$year)
#spread() is the inverse of gather.1st arg = which variable will be used as column names
#2nd arg which variable to use to fill out the cells

#Separate() and Unite()
# import data
path <- system.file("extdata", package = "dslabs")
filename <- file.path(path, "life-expectancy-and-fertility-two-countries-example.csv")
raw_dat <- read_csv(filename)
select(raw_dat, 1:5)
#it is in wide format, let's use gather to tidy it.
#not using column name year since it also includes variable type, let's call it key
dat <- raw_dat %>%
  gather(key, value, -country)
head(dat)
#not tidy bc each obs is associated w/ 2 rows instead of 1
#must separate yey column into year and variable type
#separate() takes 3 args: name of column to be separated, names to be used for new columns,
#and character that separates the variables
# separate on underscores
dat %>% separate(key, c("year", "variable_name"), "_")
#lefe_expectancy becomes just "life" so let's fix that, then spread fertility and life to create
#a column for each variable
dat <- dat %>% separate(key, c("year", "variable_name"), sep = "_", extra = "merge") %>%
  spread(variable_name, value) 
head(dat)

#S2.1 QUIZ
stats <- data.frame(key = c("allen_height", "allen_hand_length", "allen_wingspan", 
                            "bamba_height", "bamba_hand_length", "bamba_wingspan"),
                    value = c(75, 8.25, 79.25, 83.25, 9.75, 94))
head(stats)
tidy_data <- stats %>%
  separate(col = key, into = c("player", "variable_name"), sep = "_", extra = "merge") %>% 
  spread(key = variable_name, value = value)
head(tidy_data)
tidy_data <- stats %>%
  separate(col = key, into = c("player", "variable_name1", "variable_name2"), sep = "_", 
           fill = "right") %>% 
  unite(col = variable_name, variable_name1, variable_name2, sep = "_") %>% 
  spread(key = variable_name, value = value)
head(tidy_data)
tidy_data <- stats %>%
  separate(col = key, into = c("player", "variable_name"), sep = "_") %>% 
  spread(key = variable_name, value = value)
head(tidy_data)
library(dslabs)

co2
co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))
head(co2_wide)
co2_tidy <- co2_wide %>% gather(key = "month", value = co2, -year)
head(co2_tidy)
co2_plot <- co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()
co2_plot

data(admissions)
dat <- admissions %>% select(-applicants)
head(dat)

tmp <- gather(admissions, key, value, admitted:applicants)
tmp
tmp2 <- unite(tmp, column_name, c(key, gender))
tmp2
