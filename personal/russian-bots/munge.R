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

sampleColNames <- c('id', 'username', 'dump1', 'timestamp', 'retweets', 'dump2', 'favorited', 'text', 'dump3',
                    'dump4', 'hastags', 'urls', 'dump5', 'mentions', 'dump6', 'dump7')
sampleColNumbers <- c(-3, -6, -9, -10, -13, -15, -16)

##-- Functions
collectRetweets <- function(stringText){
  results <- str_match( stringText, "RT @.*?:")
  results %>%
    is.na %>%
    `!` %>%
    `[`( results, .)
} # END collectRetweets

stringText <- sampleData$text
##-- Main



##-- Play

# get the sample data
sampleData <- read_csv( file.path( directory, sampleName ), skip = 4, col_names = sampleColNames )


# clean the data of useless columns
sampleData %<>%
  select( sampleColNumbers )

# I know that trayneshacole has the most retweets so lets start with this user.
trayTweets <- sampleData %>%
  filter( username == 'trayneshacole')

# arrange based on retweet counts
traySplice <- trayTweets %>%
  arrange( desc( retweets ))

# check out text
write_csv(traySplice, path = "data\\traySplice.csv", col_names = TRUE )

# collect   
finalRetweets <- collectRetweets(sampleData$text) %>%
  as.data.table %>%
  set(j = 'count', value = 1) %>%
  as_tibble %>%
  rename( 'retweets' = '.') %>%
  group_by( retweets ) %>%
  summarize( count = n() ) %>%
  arrange( desc( count ))
  
write_csv(finalRetweets, path = 'data\\')

traySplice
