# Author: Collin Mitchell
# Date: 2018/1/28
# Purpose: Sentiment Analysis of Voltaire, Burke, Paine

library( stringr )
library( tidytext )
library( tidyverse )
library( qdap )
library( iotools )

library( rvest )

##-- Functions
######

# Iterate over and download all the files
downloadDocuments <- function( docNumbers, urlBase ){
  walk2( .x = docNumbers, .y = docNumbers, .f = function(.x, .y){
    urlTarget<- sprintf( urlBase, .x, .y)
    urlPieces <- str_split(urlTarget, "/") %>% unlist()
    filename<- urlPieces[ length( urlPieces)]
    
    download.file( url = urlTarget, destfile = file.path('data', filename), mode = "wb" )
  })
} # END downloadDocuments


# iterate over document chunks and join sentiment data
combineSentimentDocuments <- function(chunk){
  x <- dstrsplit( chunk, col_types = 'character', sep = ',')
  x$V2 <- NULL
  sentimentFrame<- inner_join(x, nrcFrame, by = c('V1' = 'word') )
  sentimentFrame$count <- 1
  sentimentFrame
} # END combineSentimentDocuments


# chunk apply over data and return sentiments per term.
collectTermsList<- function( cores = 4){
  # openfile connection
  fc <- file('data/terms.output', 'rb')
  
  # iterate over chunks
  termsList <- chunk.apply( fc, combineSentimentDocuments, CH.MAX.SIZE = 1e6, parallel = cores)
  
  # close connection
  close( fc )
  
  
  # return data
  colnames( termsList) <- c('terms', 'sentiment', 'count')
  termsList
} # END collectTermsList


# sample the data and return a ggplot object
sampleData <- function( completeDataset, sampleSize = 75000 ){
  completeDataset %>%
    sample_n( sampleSize ) %>%
    filter(!sentiment %in% c("positive", "negative")) %>%
    ggplot(aes( sentiment, fill = sentiment )) + geom_bar() + ggtitle('Voltaire\'s Emotional Content' )
}



######
##-- Variables
######

# download link for voltaire docuemnts.
urlBase <- "https://ia801406.us.archive.org/10/items/worksofvoltairec%02dvolt/worksofvoltairec%02dvolt.pdf"

# iterate through with voltaire documents
documentNumbers<- seq(3, 22, 1)


#########
##-- MAIN
#########

# get plutchik's sentiments
nrcFrame<- get_sentiments('nrc')

# download documents
downloadDocuments( documentNumbers, urlBase )

termsList <- collectTermsList()






##--- PLAY 

# load the data
data<- read_csv('data/terms.output', col_names = FALSE, progress = FALSE)[,1]
colnames( data ) <- c('terms')




results <- termsList %>%
  # Only consider Plutchik sentiments
  filter(!sentiment %in% c("positive", "negative")) %>%
  # Group by sentiment
  group_by(sentiment) %>% 
  # Get total count by sentiment
  summarize(total_count = sum(count))

termsList %>%
  # Only consider Plutchik sentiments
  filter(!sentiment %in% c("positive", "negative")) %>%
  ggplot(aes( sentiment, fill = sentiment )) + geom_bar() + ggtitle('Voltaire\'s Emotional Content' )
  
  
