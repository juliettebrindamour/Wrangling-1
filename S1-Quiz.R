library(tidyverse)
help(read_csv)
url <- "http://mlr.cs.umass.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
ex <- read_csv(url, col_names = FALSE)
ex

