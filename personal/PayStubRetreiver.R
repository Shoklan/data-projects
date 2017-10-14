# Author: Collin Mitchell
# Date: 2017/09/30
# Purpose: To download and parse paycheck information from Tops.

##-- Libraries
library( purrr )
library( rvest )
library( stringr )


##-- Constants

# load private data
setwd("~/Dropbox/Code/data-projects/personal")
load( "__data/credentials.R")




##-- Functions

# Return a session object
getSession<- function( urlTarget ){
  sessionObject <- html_session( urlTarget )
  sessionObject
}

# save credentials to a usable object
saveCredentials <- function(session, username, password){
  # credentials
  user = username
  pass = password

  # extract sesson form data.
  sessionForm<- html_form( session )[[1]]
  
  # update the form information
  completeForm<- set_values( sessionForm, "loginId" = user, "password" = pass)
  completeForm
}



##-- MAIN

# colelect a session
loginSession <- getSession( URL )

# collect the credential'd session to login.
creds <- saveCredentials( loginSession, username, password)

# collect this payment period information.
temp <- loginSession %>%
  submit_form(creds) %>%
  # move to personal page
  follow_link( css = ".intraModuleNavTab a") %>%
  # move to pay information tabular
  follow_link( "Pay Information") %>%
  # scrape paycheck details
  html_node( "#ViewPayInfo" ) 




# starting to pull apart data 
tempSlice <- xml_contents( temp )[2] %>% xml_contents()

# 9 is personal details
# 12 is paystub data
# subset important data
indexes <- c( 9, 12 )
tempSlice<- tempSlice[ indexes ]

# normalize the ex-node_set contents
extractedContents <- map( 1:length( tempSlice ), function(.x){ tempSlice[.x] %>% html_table(header = FALSE, fill = TRUE) %>% as.data.frame()})

# extract meta data
generatedRow <- data.frame( x = character(), y = character(), z = character(), stringsAsFactors = FALSE)
colnames( generatedRow )<- extractedContents[[1]][11, seq(from = 5, to = 9, by = 2)]
generatedRow[1,]<- extractedContents[[1]][11, seq(from = 6, to = 10, by = 2)]


# earnings block
earningsBlock <- extractedContents[[2]][3:14, c(1:3, 6, 7)]
colnames( earningsBlock) <- earningsBlock[1,]
earningsBlock <- earningsBlock[-1,]
earningsBlock[11, c(2,3)] <- ""
rownames(earningsBlock) <- NULL

generatedRow <- as.data.frame( c( extractedContents[[2]][31, 5], "", "", extractedContents[[2]][31, 6:7]))
colnames( generatedRow ) <- colnames( earningsBlock )
earningsBlock <- rbind( earningsBlock, generatedRow )



# tax block
taxBlock <- extractedContents[[2]][20:24, 2:4]
colnames( taxBlock ) <- c( "Tax", "Current", "Year To Date")
rownames( taxBlock )<- NULL

# union block
unionBlock <- extractedContents[[2]][27:30, 1:3]
colnames( unionBlock ) <- unionBlock[1, ]
unionBlock <- unionBlock[-1,]
rownames( unionBlock ) <- NULL


# extractedTemp <- extractedContents %>% 
#   map( str_replace_all, pattern = "[\r|\n|\t]+", replacement = " ")

