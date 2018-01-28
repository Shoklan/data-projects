# Author: Collin Mitchell
# Date: 2018/1/28
# Purpose: Sentiment Analysis of Voltaire, Burke, Paine

library( stringr )
library( tidytext )
library( tabulizer )


##-- Functions 

  ## Download the files.
urlBase <- "https://archive.org/download/worksofvoltairec%02dvolt/worksofvoltairec%02dvolt.pdf"
sprintf(urlBase, 1, 1 )

  # The file iterates.  
volumeNumbers<- seq(1, 22, 1)

  # Iterate over and download all the files
walk2( .x = volumeNumbers, .y = volumeNumbers, .f = function(.x, .y){
  urlTarget<- sprintf( urlBase, .x, .y)
  urlPieces <- str_split(urlTarget, "/") %>% unlist()
  filename<- urlPieces[ length( urlPieces)]
  
  download.file( url = urlTarget, destfile = filename)
})

## -- End DOWNLOAD FILES 


##--- PLAY 

downloadPDFs<- function( .i, .i2, fmt ){
  urlTarget<- sprintf( urlBase, 1, 1)
  urlPieces <- str_split(urlTarget, "/") %>% unlist()
  filename<- urlPieces[ length( urlPieces)]

  download.file( url = urlTarget, destfile = filename)
}