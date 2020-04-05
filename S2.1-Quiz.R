library(tidyverse)
library(dplyr)
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
