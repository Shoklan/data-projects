# Author: Collin Mitchell
# Date: 2018/1/28
# Purpose: Sentiment Analysis of Voltaire, Burke, Paine

library( stringr )
library( tidytext )
library( tabulizer )
library( tidyverse )
library( qdap )
library( iotools )

##-- Functions


  ## Download the files.
urlBase <- "https://archive.org/download/worksofvoltairec%02dvolt/worksofvoltairec%02dvolt.pdf"

  # The file iterates.  
volumeNumbers<- seq(1, 22, 1)

  # Iterate over and download all the files
walk2( .x = volumeNumbers, .y = volumeNumbers, .f = function(.x, .y){
  urlTarget<- sprintf( urlBase, .x, .y)
  urlPieces <- str_split(urlTarget, "/") %>% unlist()
  filename<- urlPieces[ length( urlPieces)]
  
  download.file( url = urlTarget, destfile = file.path('data', filename) )
})

## -- End DOWNLOAD FILES 




##--- PLAY 

# load the data
data<- read_csv('terms.output', col_names = FALSE, progress = FALSE)[,1]
colnames( data ) <- c('terms')

nrcFrame<- get_sentiments('nrc')

play <- function(chunk){
  x <- dstrsplit( chunk, col_types = 'character', sep = ',')
  x$V2 <- NULL
  sentimentFrame<- inner_join(x, sentimentFrame, by = c('V1' = 'word') )
  sentimentFrame$count <- 1
  sentimentFrame
}

fc <- file('terms.output', 'rb')

termsList <- chunk.apply( fc, play, CH.MAX.SIZE = 1e6, parallel = 4)
close( fc )
termsList

colnames( termsList) <- c('terms', 'sentiment')

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

