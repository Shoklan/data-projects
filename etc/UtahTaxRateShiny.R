# Author: Collin Mitchell
# Date: 2017/10/04
# Purpose: To be an example of Shiny.


##- Libraries ##
library( rvest )
library( purrr )
library( rio )


##-- Variables ##

# url to reach out to
baseurl <- "http://tax.utah.gov/salestax/rate/%sq%ssimple.xls"

# static url components for period.
quarterStrings<- sprintf( "%02d", sort( c( c(9, 9, 9), rep( 10:17, times = 4))))
quarterCycle<- rep(1:4, times = 9 )[-1]

# compile URLs
generatedURLs<- sprintf( baseurl, quarterStrings, quarterCycle )

# collect file names
filenames<- unlist( map( str_split(generatedURLs, "/"), 6))


##- Main ##


# download all the data
map2( .x = generatedURLs, .y = filenames, .f = download.file, mode = "wb" )
data <- import( filenames[1])

