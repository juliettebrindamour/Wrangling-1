library(tidyverse)
library(dslabs)
library(readxl)
#importing spreadsheets
getwd()
path <- system.file("extdata", package="dslabs")
list.files(path)
filename <- "murders.csv"
fullpath <- file.path(path, filename)
fullpath
file.copy(fullpath, getwd())
file.exists(filename)
#learning readr and readxl
#reading the first 3 lines of the file to inspect it
read_lines("murders.csv", n_max = 3)
#read file in csv format
dat <- read_csv(filename)
#read using full path
dat <- read_csv(fullpath)
head(dat)
#reading using r base functions instead of tidyverse
dat2 <- read.csv(filename, stringsAsFactors = FALSE) #so that characters stay characters
head(dat2)
#downloading files from the internet
url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
dat <- read_csv(url)
download.file(url, "murders.csv")
#temporary file names
tempfile()
tmp_filename <- tempfile()
download.file(url, tmp_filename)
dat <- read_csv(tmp_filename)
file.remove(tmp_filename)

