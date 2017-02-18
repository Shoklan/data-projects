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

library(tidyverse)
library( rvest )
library( stringi )
library(SOAR)

Store()

## -- Varibales ---##
filepath<- file.path("Code", "data-projects", "wikileaks-hbgary-emails")
MAX_EMAIL_COUNT <- 71794
url <- 'https://wikileaks.org/hbgary-emails/emailid/'


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
  print( paste0("Reached count: ", index ))
  
  completeUrl<- paste0(url, index)
  urlhtml<- read_html(completeUrl)
  emailText<- urlhtml %>%  html_nodes(".email-content") %>% html_text()
  
  
  emailTemp<- unlist(strsplit( emailText, "\n"))
  
  emailData<- unlist(map(emailTemp,  gsub, pattern = "^>|\t|\n", replacement = ""))
  emailData<- unlist(map( emailData, stri_trim))
  emailData<-  emailData[ which(emailData != "") ]
  rm(urlhtml)
  gc()
  emailData
}

# Test function
# wikileaksEmailScrape(1, url = url)


##-- Main --##
collectedEmails <- safely(map( 1:MAX_EMAIL_COUNT, wikileaksEmailScrape, url = url))
Store( collectedEmails )

write.csv(collectedEmails, file = paste0(filepath, "/CollectedEmails.csv" ))
save( collectedEmails, file = paste0(filepath, "collectedEmails.RData" ))
