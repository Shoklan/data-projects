# Author: Collin Mitchell
# Date: 2017/10/04
# Purpose: To be an example of Shiny.


##- Libraries ##
library( rvest )
library( purrr )
library( stringr )
# library( rio )
library( xlsx )
library( ggplot2 )

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

# columns to collect
dataColumnIndexes <- c(1, 3:9)
columnHeaders <- c("Location", "CityCode", "SalesAndUse", "Food", "TransientRoom", "Restaurant", "Leasing", "ResortTax")

# create container for boolean values
boolVector <- logical(nrow( data ))

##- Functions ##
findEmptyRows <- function(.x, .i ){
  sum( is.na( .x[.i, ])) == 8
}


##- Main ##


# download all the data
walk2( .x = generatedURLs, .y = filenames, .f = download.file, mode = "wb" )
# data <- import( filenames[1])
data <- read.xlsx(filenames[1], sheetIndex = 1, startRow = 12,
                  header = FALSE, colIndex = dataColumnIndexes)

# fix column names
colnames( data ) <- columnHeaders

# find empty rows
for( i in 1:nrow( data )){
  boolVector[i] <- findEmptyRows(data, i)
}
deleteIndexes <- which( boolVector == TRUE)

# delete them
data <- data[-deleteIndexes,]

# find description text at bottom and delete
bottomIndex <- grep( "Sales of motor vehicles", data$Location)
bottomIndex <- c((bottomIndex-1):nrow( data ))
data <- data[-bottomIndex,]


coord_map()
temp <- map_data("county")
tempTwo <- subset( temp, region == "utah")
