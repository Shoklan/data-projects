# Author: Collin Mitchell
#  Date: 2018/02/21
# Purpose Explore 'Russian Bots' Dataset

##-- Libraries 
library( tidyverse )
library( stringr )
library( tidytext )
library( magrittr )
library( data.table )

##-- Variables
directory  <- 'data'
sampleName <- 'sample.csv'
tweetsName <- 'tweets.csv'
usersName  <- 'users.csv'

# PDF url: https://democrats-intelligence.house.gov/uploadedfiles/exhibit_b.pdf


##-- Functions

# read sample data
readSample <- function(){
  
  # prepatory information
  targetFile <- file.path( directory, sampleName)
  replacement_names <- c('id', 'username', 'dump1', 'timestamp', 'retweets', 'dump2', 'favorited', 'text', 'dump3',
                      'dump4', 'hastags', 'urls', 'dump5', 'mentions', 'dump6', 'dump7')
  sampleColNumbers <- c(-3, -6, -9, -10, -13, -15, -16)
  
  data <- read_csv(targetFile, col_names = replacement_names, skip = 4) %>% 
    select( sampleColNumbers )
  
    
} # END readSample

# read tweets data
readTweets <- function(){
  
  # prepatory information
  targetFile <- file.path( directory, tweetsName)
  replacement_names <- c("userID", "username", "created",
                         "timestamp", "retweeted", "retweet_boolean",
                         "favorited", "text", "twwetID",
                         "source", "hashtags", "urls", 
                         "posted", 'mentions', 'retweet_id',
                         'replyTo')
  
  data <- read_csv( targetFile, col_names = replacement_names ) %>%
    select( username, created, timestamp, retweeted, favorited, text, hashtags, mentions, replyTo)
  
} # END readTweets


# read username data
readUsername <- function(){
  
  # prepatory information
  targetFile <- file.path( directory, usersName)
  replacement_names <- c('userID', 'location', 'displayName',
                         'followers', 'statuses', 'timezone',
                         'verified', 'language', 'username',
                         'description', 'creation', 'favorited',
                         'friends', 'inList')
  data <- read_csv( targetFile, col_names = replacement_names )
} # END readUsername











##-- Main

sampleData <- readSample()
tweetData <- readTweets()
userData <- readUsername()

# number of rows per dataset
map( list( sampleData, tweetData, userData), nrow) %>% unlist


# figure out who was retweeted the most
rts <- tweetData %>%
  select( text ) %>%
  str_extract_all( "RT @.*?:") %>% 
  unlist %>% 
  tibble( rt = ., count = 1) %>%
  group_by( rt ) %>%
  summarise( total = sum( count )) %>%
  arrange( desc( total ))


# figure out how many russian accounts existed.
rts %>%              
  top_n( 20 ) %>%    # find top 20 users
  pull( rt ) %>%     # pull into vector
  str_replace(pattern = "RT @", replacement = "") %>%                                   # replace and clean names
  str_replace(pattern = ":", replacement = "") %in% {userData$username %>% unique}  %>% # find who exists in user data
  `[`( x =(rts %>% top_n( 20 )), i = ., j=1)


##-- Play

# collect offending accounts
russianAccts <- rts %>%              
  top_n( 100 ) %>% 
  pull( rt ) %>%
  str_replace(pattern = "RT @", replacement = "") %>%                                   
  str_replace(pattern = ":", replacement = "") %in% {userData$username %>% unique}  %>% 
  `[`( x =(rts %>% top_n( 100 )), i = ., j=1) %>%
  pull( rt ) %>%
  str_replace(pattern = "RT @", replacement = "") %>%                                   
  str_replace(pattern = ":", replacement = "")

# how old are these accounts?
userData %>%
  filter( username %in% russianAccts ) %>%
  select( username, creation, friends )

# check for accounts that are lacking creation date - ie purged accts.
russianAccts <- userData %>%
  filter( is.na( creation )) %>%
  pull( username )

# 1% of all tweets?!?
russianTweets <- tweetData %>%
  filter( username %in% russianAccts )



# sentiment of tweets at DT and HRC
str_detect(tweetData$text, '@realDonaldTrump|@HillaryClinton') %>% length



