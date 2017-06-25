# Author: Collin Mitchell
# Date: 2017/6/25
# Purpose: Exploration of Data about Proposed amendments to the Constitution


##> Libraries <##
library( rio )
library( dplyr )
library( purr )
library( data.table )
library( ggplot2 )

##> variabels <##
dataPath <- file.path( "data", "us-nara-amending-america-dataset-raw-2016-02-25.csv")
data<- import( dataPath )

# make sure codepage and data match
colnames( data )

# can't use since most are considered numbers
summary( data )

# Columns I don't care about:
# identifier
# source_code
# source_citation
# source_index_number
# congressional_session
# joint_resolution_chamber
# joint_resolution_number
# committee_of_referral
# last_modified

# rename:
  # state<- sponsor_state_or_territory
  # individual<- sponsor_name
  # approx<- date_approximation
  # purpose <- title_or_description_from_source

# Used this to check which columns contain data I think I'll use.
map(data, unique )


# convert to data.table for quick conversion
data <- as.data.table( data )

# clean data
dataSlice <- 
  data %>%
  rename( state = sponsor_state_or_territory, individual = sponsor_name,                 # correct column names
          approx = date_approximation, purpose = title_or_description_from_source) %>%
  select( state, individual, approx, purpose, year, month, day, congress ) %>%           # only keep there columns
  mutate(year = as.numeric( year ),                                                      # convert to numbers
         month = as.numeric( month ),
         day = as.numeric( day ),
         congress = as.numeric( congress )) %>%
  filter( !is.na( year ))

# ensure that types were changed correctly
map( dataSlice, typeof )


# there is a type in the data; max year is 19931
summary( dataSlice )

# correct this.
index<- which( dataSlice$year == 19931 )
dataSlice$year[ index ] <- 1931

# prepare graph
ggplot( dataSlice, aes( year )) + geom_histogram(bins = 23 )
# needs work

# create cut factors
extremes<- range( dataSlice$year, na.rm = TRUE )
cutValues<- c( seq( extremes[1], extremes[2], by = 10), extremes[2])


labels<- c( unlist(map( .x = 1:length( cutValues )-1, .f = function(x){
  mean( cutValues[x], cutValues[x+1])
}))[-1])

# insert factors for graphing.
dataSlice$year<- cut(dataSlice$year, breaks = cutValues, labels = labels, ordered_result = TRUE)
ggplot( dataSlice, aes( year, fill = year )) + geom_histogram( stat =  "count")


