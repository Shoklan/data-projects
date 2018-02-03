# Author: Collin Mitchell
# Date: 2017/02/15
# Purpose: Convert HBGary Emails to R List Data

##----- Overview ---------##
# download data
# played with rvest since I'm rusty
# figured out how many entires there are
# break text via new lines
# then remove new line and tab characters.
# # trim extra space
# # delete empty lines
# build function for process.
# SAVE!

# Notes:
# delete objects to destory open connections.

library( tidyverse )
library( rvest )
library( stringr )
library( curl)


## -- Varibales ---##
filepath<- file.path("Code", "data-projects", "wikileaks-hbgary-emails")
MAX_EMAIL_COUNT <- 71794
url <- 'https://wikileaks.org/hbgary-emails/emailid/'
urlSessionHome<- 'https://wikileaks.org'

# TEST STUFF
# urlhtml<- read_html(url)
# emailText<- urlhtml %>%  html_nodes(".email-content") %>% html_text()

# emailTemp<- unlist(strsplit( emailText, "\n"))

# emailData<- unlist(map(temp,  gsub, pattern = "^>|\t|\n", replacement = ""))
# emailData<- unlist(map( emailData, stri_trim))
# emailData<-  emailData[ which(emailData != "") ]

##-- Functions --#

# Download the data and return a cleaned list
wikileaksEmailScrape <- function( index, url){
  # print( paste0("Reached count: ", index ))
  
  
  completeUrl<- paste0(url, index)
  # urlhtml<- read_html(completeUrl)
  
  #while( !exists( quote( "urlhtml" ))){
    # urlhtml<- read_html( curl(url = completeUrl, handle = curl::new_handle("useragent" = "Mozilla/5.0")))
  #  urlhtml<- read_html( completeUrl, options = "NOERROR")
    
  #  Sys.sleep( 2 )
  #  print( paste0( "Does urlhtml exists: ", exists(quote( "urlhtml" ))))
  #}
  
  completedSuccessfully = FALSE
  repeat({
    completedSuccessfully <- tryCatch(
      expr = {
        emailText <- sessionObject %>% 
        jump_to( paste0(url, index) ) %>%
        html_nodes(".tab-content") %>%
        html_text()
        },
      error = function(e){ Sys.sleep(60); completedSuccessfully = FALSE}
    )
    print("loop")
    if( completedSuccessfully != FALSE ) break
  })
  
  
  
  emailTemp<- unlist(strsplit( emailText, "\n"))
  str_replace_all( emailTemp, pattern = '^>+|\t+', replacement = "")
  str_detect(emailTemp, '\"\"')
  
  emailTemp <- 
  str_replace( emailTemp)
  
  
  emailData<- unlist(map( emailData, stri_trim))
  emailData<-  emailData[ which(emailData != "") ]
  
  emailData
}

# Return a session object
getSession<- function( urlTarget ){
  sessionObject <- html_session( urlTarget )
  sessionObject
}

# Test function
# wikileaksEmailScrape(1, url = url)


##-- Main --##

sessionObject<- getSession( urlSessionHome )

collectedEmails <- safely(map( 1:MAX_EMAIL_COUNT, wikileaksEmailScrape, url = url))

write.csv(collectedEmails, file = paste0(filepath, "/CollectedEmails.csv" ))
save( collectedEmails, file = paste0(filepath, "collectedEmails.RData" ))
