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

#S2.3 QUIZ
#Loading webpage w/ info about baseball players
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
#extracting first table using table nodes
nodes <- html_nodes(h, "table")
#converting first 4 tables to data frames
get_table <- function(n) {
  t <- html_table(nodes[[n]])
  t
}
t1 <- get_table(1)  
t1
t2 <- get_table(2)
t2
t3 <- get_table(3)
t3
t4 <- get_table(4)
t4
#finding last 3 entries of nodes
length(nodes)
t19 <- get_table(19)
t19
t20 <- get_table(20)
t20
t21 <- get_table(21)
t21

#creating tables
tab_1 <- get_table(10)
tab_2 <- get_table(19)
#removing unnecessar column from tab_1
head(tab_1) #observe
tab_1 <- tab_1 %>% select(X2, X3, X4)
head(tab_1) #check if it worked (yes)
#editing tables before join
tab_1 <- data.frame("Team" = tab_1$X2[2:31], "Payroll" = tab_1$X3[2:31], 
                    "Average" = tab_1$X4[2:31])
tab_1
tab_2 <- data.frame("Team" = tab_2$X1[2:31], "Payroll" = tab_2$X2[2:31], 
                    "Average" = tab_2$X3[2:31])
tab_2
#joining them
new <- full_join(tab_1, tab_2, by = "Team")
new

#brexit questions
library(rvest)
library(tidyverse)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_
United_Kingdom_European_Union_membership_referendum&oldid=896735054"
#assigning tab to be the html nodes of the table class
i <- read_html(url)
tab <- html_nodes(i, "table")
length(tab)
#inspecting
get_head <- function(n) {
  t <- html_table(tab[[n]], fill = TRUE)
  head(t)
}
get_head(1)
get_head(2)
get_head(3)
get_head(4)
get_head(5)

