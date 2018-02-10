# Author: Collin Mitchell
# Date: 2018/1/28
# Purpose: Sentiment Analysis of Voltaire, Burke, Paine

library( tidytext )
library( tidyverse )
library( iotools )

##-- Functions
######

# Iterate over and download all the files
downloadDocuments <- function( docNumbers, urlBase){
  walk2( .x = docNumbers, .y = docNumbers, .f = function(.x, .y){
    urlTarget<- sprintf( urlBase, .x, .y)
    urlPieces <- str_split(urlTarget, "/") %>% unlist()
    filename<- urlPieces[ length( urlPieces)]
    
    download.file( url = urlTarget, destfile = file.path('data', filename), mode = "wb" )
  })
} # END downloadVoltaireDocuments




# clean the work directory
clearDirectory<- function(){
  walk( file.path( 'data', list.files( 'data')), file.remove)
}





# iterate over document chunks and join sentiment data
combineSentimentDocuments <- function(chunk){
  x <- dstrsplit( chunk, col_types = 'character', sep = ',')
  x$V2 <- NULL
  x<- tolower(x)
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
sampleData <- function( completeDataset, sampleSize = 75000, graphTitle ){
  completeDataset %>%
    sample_n( sampleSize ) %>%
    filter(!sentiment %in% c("positive", "negative")) %>%
    ggplot(aes( sentiment, fill = sentiment )) + geom_bar() + ggtitle( graphTitle )
} # END sampleData



# 
combinePaineSentimentDocs <- function( .x ){
  textData<- readLines( .x )
  
  tempText<- textData %>%
    str_split(' ') %>%
    unlist() %>%
    map( str_replace_all, pattern = '\\W', replacement = '' ) %>%
    unlist() %>%
    as_tibble() %>%
    tail(., nrow(.) - 61 ) %>%
    mutate(count = 1)
  
  colnames( tempText ) <- c('terms', 'count')
  
  result<-tempText %>%
    inner_join( nrcFrame, by = c('terms' = 'word') ) %>%
    # Only consider Plutchik sentiments
    filter(!sentiment %in% c("positive", "negative"))
  
  result
} # END combinePaineSentimentDocs



######
##-- Variables
######

# download link for voltaire docuemnts.
urlVoltaireBase <- "https://ia801406.us.archive.org/10/items/worksofvoltairec%02dvolt/worksofvoltairec%02dvolt.pdf"
urlPaineBase<- "http://www.gutenberg.org/cache/epub/374%d/pg374%d.txt"


# iterate through with voltaire documents
voltaireDocumentNumbers<- seq(3, 22, 1)

# iterate through with paine documents
paineDocumentNumbers<- 1:3






#########
##-- MAIN
#########

# get plutchik's sentiments
nrcFrame<- get_sentiments('nrc')

## Voltaire documents
# download voltaires
downloadDocuments( voltaireDocumentNumbers, urlVoltaireBase )

# create sentiment frame for analysis
termsList <- collectTermsList()

# generate Voltaire's Sentiments.
voltaireGraph <- sampleData( termsList, graphTitle = "Voltaire's Sentiments" )
## END VOLTAIRE


## Thomas Paine documents
downloadDocuments( paineDocumentNumbers, urlPaineBase )

files<- file.path( 'data', list.files('data'))

termsList<- map( files, combinePaineSentimentDocs ) %>% bind_rows()
paineGraph <- sampleData( termsList, sampleSize = 10000, graphTitle = "Thomas Paine's Sentiments")

paineGraph








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
  
  
