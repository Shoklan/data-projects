# Author: Collin Mitchell
# Date: 2018/1/28
# Purpose: Sentiment Analysis of Voltaire, Burke, Paine

library( tidytext )
library( tidyverse )
library( iotools )

##-- Functions
######

# clean the work directory
clearDirectory<- function(){
  saveDir<- getwd()
  fileTargets <- base::setdiff(
    list.files( 'data', full.names = FALSE ),
    list.dirs('data', recursive = FALSE, full.names = FALSE)
  )
  
  setwd('data')
  walk(fileTargets, file.remove)
  
  setwd(saveDir)
  
} # END clearDirectory

# Iterate over and download all the files
downloadDocuments <- function( docNumbers, urlBase){
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
  x$V1<- tolower( x$V1 )
  
  sentimentFrame<- x %>% inner_join(nrcFrame, by = c('V1' = 'word') )
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
sampleData <- function( completeDataset, sampleSize = 75000){
  completeDataset %>%
    # sample data
    sample_n( sampleSize ) %>%
    # remove columns for positive, negative since it's not relevant
    filter( !sentiment %in% c('negative', 'positive') ) %>%
    group_by( sentiment ) %>%
    # calculate the percentage of each sentiment
    summarize( percentage =  n() / nrow( completeDataset %>% filter( !sentiment %in% c('negative', 'positive'))) )

} # END sampleData

# create and return the grpah.
generateGraph <- function( data, graphTitle ){
  data %>%
  # generate graph
    ggplot( aes( x = sentiment, y = percentage, fill = sentiment) ) +
    geom_col() +
    ggtitle( graphTitle )
} # END generateGraph



######
##-- Variables
######

# download link for docuemnts.
urlVoltaireBase <- "https://ia801406.us.archive.org/10/items/worksofvoltairec%02dvolt/worksofvoltairec%02dvolt.pdf"
urlPaineBase    <- "https://ia802704.us.archive.org/0/items/lifewritingsofth%02dpain/lifewritingsofth%02dpain.pdf"
urlBurkeBase <- "https://ia802701.us.archive.org/10/items/burkeworks%02dburkuoft/burkeworks%02dburkuoft.pdf"



# iterate through with voltaire documents
voltaireDocumentNumbers<- 4:22

# iterate through with paine documents
paineDocumentNumbers<- 3:9

# iterate through with burke documents
burkeDocumentNumbers <- 1:6
burkeDocumentNumbers <- burkeDocumentNumbers[-3]





#########
##-- MAIN
#########

# get plutchik's sentiments
nrcFrame<- get_sentiments('nrc')

## Voltaire documents
# download voltaires
downloadDocuments( voltaireDocumentNumbers, urlVoltaireBase )

# Run parsing script
system2('python3', 'script.py')

# create sentiment frame for analysis
voltaireTermsList <- collectTermsList()

# generate Voltaire's Sentiments.
voltaireTibble <- sampleData( voltaireTermsList )
voltaireGraph <- generateGraph( voltaireTibble , graphTitle = "Voltaire's Sentiments" )
## END VOLTAIRE

clearDirectory()

## Thomas Paine documents
# download paines
downloadDocuments( paineDocumentNumbers, urlPaineBase )

# Run parsing script
system2('python3', 'script.py')

# create sentiment frame for analysis
paineTermsList <- collectTermsList()

paineTibble <- sampleData( paineTermsList  )
paineGraph <- generateGraph( paineTibble, graphTitle = "Paine's Sentiments")
## END PAINE

clearDirectory()

## Edmond Burke documents
# download burke
downloadDocuments( burkeDocumentNumbers, urlBurkeBase)

# Run parsing script
system2('python3', 'script.py')

# create sentiment frame for analysis
burkeTermsList <- collectTermsList()

burkeTibble <- sampleData( burkeTermsList )
burkeGraph <- generateGraph( burkeTibble, graphTitle = "Burke's Sentiments")






##--- PLAY 

# load the data
data<- read_csv('data/terms.output', col_names = FALSE, progress = FALSE)
colnames( data ) <- c('terms')

voltaireTermsList %>%
  # filter on sentiment
  filter( !sentiment %in% c('negative', 'positive') ) %>%
  group_by( sentiment ) %>%
  summarize( percentage =  n() / nrow( voltaireTermsList %>% filter( !sentiment %in% c('negative', 'positive'))) ) %>%
  ggplot( aes( x = sentiment, y = percentage, fill = sentiment) ) +
  geom_col()
  
  

