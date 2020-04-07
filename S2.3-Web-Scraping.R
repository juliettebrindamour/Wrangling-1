library(dslabs)
library(dplyr)
library(tidyverse)
#Web Scraping
#html_nodes() selects the class of object you want from the html code of a webpage (ex. tables)
# import a webpage into R
library(rvest)
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
h <- read_html(url)
class(h)
h

tab <- h %>% html_nodes("table")
tab <- tab[[2]]

tab <- tab %>% html_table
class(tab)

tab <- tab %>% setNames(c("state", "population", "total", "murders", "gun_murders", 
                          "gun_ownership", "total_rate", "murder_rate", "gun_murder_rate"))
head(tab)

#CCS Selectors
#Example extracting recipe name, total prep time and ingredient list from website
#Note that it is hard to know what selector to use in the html_nodes() function
#For this there is a SelectorGadget available on Google Chrome
h <- read_html("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")
recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()
#now that we have our selectors we want to create a list
guacamole <- list(recipe, prep_time, ingredients)
guacamole
#The layout on this website is always the same, let's create a function that works on any page
get_recipe <- function(url){
  h <- read_html(url)
  recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
  prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
  ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()
  return(list(recipe = recipe, prep_time = prep_time, ingredients = ingredients))
} 
#Testing it out
get_recipe("http://www.foodnetwork.com/recipes/food-network-kitchen/pancakes-recipe-1913844")
