library(rvest)
library(tidyverse)
library(dplyr)
library(dslabs)
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
