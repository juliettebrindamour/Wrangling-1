library(dslabs)
library(dplyr)
library(tidyverse)
library(readr)
#DATES AND TIMES
# inspect the startdate column of 2016 polls data, a Date type
data("polls_us_election_2016")
polls_us_election_2016$startdate %>% head
class(polls_us_election_2016$startdate) #class is "date"
as.numeric(polls_us_election_2016$startdate) %>% head

# ggplot is aware of dates
polls_us_election_2016 %>% filter(pollster == "Ipsos" & state =="U.S.") %>%
  ggplot(aes(startdate, rawpoll_trump)) +
  geom_line()

# lubridate: the tidyverse date package (used to handle dates)
library(lubridate)

# select some random dates from polls (to show what we can do with lubridate)
set.seed(2)
dates <- sample(polls_us_election_2016$startdate, 10) %>% sort
dates

# extract month, day, year from date strings w/ year() month() and day()
data.frame(date = dates, 
           month = month(dates),
           day = day(dates),
           year = year(dates))

month(dates, label = TRUE)    # extract month label with label = TRUE argument

# ymd works on mixed date styles and returns dates with format YYYY-MM-DD
x <- c(20090101, "2009-01-02", "2009 01 03", "2009-1-4",
       "2009-1, 5", "Created on 2009 1 6", "200901 !!! 07")

ymd(x)

# different parsers extract year, month and day in different orders
x <- "09/01/02"
ymd(x)
mdy(x)
ydm(x)
myd(x)
dmy(x)
dym(x)

#Lubridate also deals with times
now()    # current time in your time zone
now("GMT")    # current time in GMT
now() %>% hour()    # current hour
now() %>% minute()    # current minute
now() %>% second()    # current second

# parse time
x <- c("12:34:56")
hms(x)

#parse datetime
x <- "Nov/2/2012 12:34:56"

mdy_hms(x)

#TEXT MINING
#CASE STUDY: TRUMP TWEETS
library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)
set.seed(1)

#getting Trump's Tweet data
url <- 'http://www.trumptwitterarchive.com/data/realdonaldtrump/%s.json'
trump_tweets <- map(2009:2017, ~sprintf(url, .x)) %>%
  map_df(jsonlite::fromJSON, simplifyDataFrame = TRUE) %>%
  filter(!is_retweet & !str_detect(text, '^"')) %>%
  mutate(created_at = parse_date_time(created_at, orders = 
                                        "a b! d! H!:M!:S! z!* Y!", tz="EST")) 

library(dslabs)
data("trump_tweets")

#examining the data
head(trump_tweets)
names(trump_tweets) #which variables are given
#the text variable shows us the content of the tweet 
#the source variable shows us the device used to compose and upload the tweet
trump_tweets %>% count(source) %>% arrange(desc(n))

#Let's use extract() to remove "Twitter" from the source names
trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  count(source) 

#Let's focus on what was tweeted during the presidential campaign by selecting the
#corresponding dates into a table (Apple = staf, Android = DJT). Also removing retweets
campaign_tweets <- trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  filter(source %in% c("Android", "iPhone") &
           created_at >= ymd("2015-06-17") & 
           created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>%
  arrange(created_at)
head(campaign_tweets)

#let's look at the proportion of tweets posted at each hour for each device (data visualization)
ds_theme_set()
campaign_tweets %>%
  mutate(hour = hour(with_tz(created_at, "EST"))) %>%
  count(source, hour) %>%
  group_by(source) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "")
#the differing patterns in timing of tweets confirm that two different entities are 
#tweeting from each device

#Let's now study the content of the tweets using the tidytext package (text as data)
install.packages("tidytext")
library(tidytext)
#helps us convert text into a tidy table
#main function required for this is unnest_tokens()
  #token = units we are considering to be a data point ex words
  #function extracts tokens from a vector of strings. Each token gets row in new table
example <- data_frame(line = c(1, 2, 3, 4),
                      text = c("Roses are red,", "Violets are blue,", "Sugar is sweet,", 
                               "And so are you."))
example
example %>% unnest_tokens(word, text)
#other example with tweet #3008
i <- 3008
campaign_tweets$text[i]
campaign_tweets[i,] %>% 
  unnest_tokens(word, text) %>%
  select(word)

#the default token (word) does not include @ or # which are important to twitter
#let's make a pattern that does
pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"

#we can now appropriately extract hashtags and mentions
campaign_tweets[i,] %>% 
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)

#another adjustment we want is to remove links to pictures
campaign_tweets[i,] %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)

#now ready to extract words to all tweets
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) 

#what are the most commonly used words?
tweet_words %>% 
  count(word) %>%
  arrange(desc(n))

#the stop_word datat base includes insignificant words which are most used
#let's filter these out
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word ) 

#more informative set of top 10 most tweted words
tweet_words %>% 
  count(word) %>%
  top_n(10, n) %>%
  mutate(word = reorder(word, n)) %>%
  arrange(desc(n))

#more adjustments to the code so it runs smoother
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word &
           !str_detect(word, "^\\d+$")) %>%
  mutate(word = str_replace(word, "^'", ""))

#which words are more common to iphone / android
android_iphone_or <- tweet_words %>%
  count(word, source) %>%
  spread(source, n, fill = 0) %>%
  mutate(or = (Android + 0.5) / (sum(Android) - Android + 0.5) / 
           ( (iPhone + 0.5) / (sum(iPhone) - iPhone + 0.5)))
android_iphone_or %>% arrange(desc(or))
android_iphone_or %>% arrange(or)

#limiting our analysis to high frequency words
android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(desc(or))

android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(or)

#sentiment analysis (object sentiment associates words with sentiments)
sentiments

#The AFINN lexicon assigns a score between -5 and 5, 
#with -5 the most negative and 5 the most positive.
get_sentiments("afinn")

#i quit

#SECTION 4 QUIZ
library(dslabs)
library(lubridate)
options(digits = 3)    # 3 significant digits

data(brexit_polls)
head(brexit_polls)
#start date in april
start <- arrange(brexit_polls, startdate)
start

end <- brexit_polls %>% mutate(enddate = weekdays(enddate)) %>% arrange(enddate)
end

data(movielens)
movielens2 <- movielens %>% mutate(timestamp = as_datetime(timestamp)) %>% 
  mutate(yr = year(timestamp), hour = hour(timestamp)) %>% group_by(hour) %>%
  summarise(n = length(hour))
movielens2
max(movielens2$n)
filter(movielens2, n==7011)

#Gutenberg questions
install.packages("gutenbergr")
library(tidyverse)
library(gutenbergr)
library(tidytext)
options(digits = 3)
library(dplyr)

#view what is on gutenbergr
head(gutenberg_metadata)

#finding ID number for pride and prejudice
ppid <- gutenberg_metadata %>% mutate(ispp = str_detect(gutenberg_metadata$title, "Pride and Prejudice")) %>% filter(ispp == TRUE)
ppid

#gutenberg_works() function 
?gutenberg_works
gutenberg_works(title == "Pride and Prejudice")

#downolading pride and prejudice
pptext <- gutenberg_download(1342)
#tidying it up, counting the words w/out stop words and digits
tidypp <- pptext %>% unnest_tokens(words, text) %>% filter(!words %in% stop_words$word) %>% 
  mutate(isdigits = str_detect(tidypp$words, "\\d")) %>% filter(!isdigits == TRUE)
length(tidypp$words)
#finding most used words
mostused <- tidypp %>% 
  count(words) %>%
  mutate(word = reorder(words, n)) %>%
  arrange(desc(n)) %>% filter(n >= 100)
print(mostused)
#lexicon
install.packages("textdata")
afinn <- get_sentiments("afinn")
afinn

common <- tidypp$words %in% afinn$word %>% which() %>% length()
common <- inner_join(tidypp, afinn, by = "word")
