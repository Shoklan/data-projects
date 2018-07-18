# Author: Collin Mitchell
# Date: 2018/07/18
# Purpose: To be an example of building keywords for pages


##-- Libraries
library( tidyverse )
library( rvest )
library( tm )
library( tidytext )
library( xml2 )
library( tokenizers )

## Varibales
urlTarget <- "https://kb.datto.com/hc/en-us/articles/200555585-Getting-Started-with-the-ShadowSnap-Agent"
nObjects <- 50 ## Change this to alter the number of values to output
stopwords <- stopwords("en") %>% tibble( word = .)

##-- Functions

# Return a session object
getSession<- function( urlTarget ){
  sessionObject <- html_session( urlTarget )
  sessionObject
}


# collect the title of the article
getArticleTitle <- function( sessionObject ){
   sessionObject %>%
    read_html %>%
    html_nodes( ".banner-title" ) %>%
    xml_text
} ## END getArticleTitle


# collect the words from the article
getWordsFromArticle <- function( sessionObject, numberOfWords = 50 ){
  sessionObject %>%
    read_html %>%
    html_nodes(".article-body") %>%
    xml_text %>%
    tokenize_words( simplify = TRUE ) %>%
    tibble(word = .) %>%
    left_join( stopwords ) %>%
    mutate( count = 1) %>%
    count( word, sort = TRUE) %>%
    arrange( n ) #%>%
    #top_n( numberOfWords )
} ## END getWordsFromArticle


writeCSVFile <- function( wordsDF ){
  wordsDF %>%
    write_csv(path = "top50.csv")
} ## END writeCSVFile


##-- Main

# collect the session for future communications
session <- getSession( urlTarget )

# collect the article title
articleTitle <-getArticleTitle( session )

# get the words
collectWords <- getWordsFromArticle( session, nObjects )

# output the file for reference
writeCSVFile( collectWords )




##-- Play 

# check for title consistentcy
articleTitle <- session %>%
  read_html %>%
  html_nodes( ".banner-title" ) %>%
  xml2::xml_text()



tmp <- session %>%
  read_html %>%
  html_nodes(".article-body") %>%
  xml_text %>%
  tokenize_words( simplify = TRUE ) %>%
  tibble(word = .) %>%
  left_join( stopwords )


tmp$word[ str_detect(tmp$word, "gigabit" )] %>% sum
