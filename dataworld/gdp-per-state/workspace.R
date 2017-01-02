#   Author: Collin Mitchell
#     Date: 2016/12/30
# fin-Date: 2017/1/1
#     Role: gdp-per-state Exploratory Analysis

# Initial thoughts.
# Trend per state
# Trend per region

# Packages
library( rio )
library( tidyverse )
library( data.table )
library( feather )
library( reshape2 )

# Correct path
path<- file.path( 'Code', 'data-projects', 'dataworld', 'gdp-per-state')
setwd( path )

# collect data
data<- data.table( import( file.path('data', 'GDPperCapita.xls' ) ))

################################################
##### this was for outputting extra file formats; included in repo
# files<- paste0( 'data/GDPperCapita', c( '.csv', '.feather', '.rds', '.sav', '.sas7bdat' ))

# invisible( sapply( files, function(x){
#  export( data, x)
# ))
################################################

####################
#|  Correct Data   |
####################


# Grab State/Region Labels
states <- data$V1
data[, V1 := NULL]

# Transpose to correct and collect Years
temp<- t( data )
years<- as.numeric( rownames( temp ))

# Convert Matrix -> Data.Table
data<- data.table( temp )


# Finalize into Tidy Format
colnames( data )<- states
data[, c('V61', 'V62', 'V63', 'V64') := NULL]

# This runs without issues; ignore warnings
suppressWarnings( data[, Year := years ] )

# Regions of interest
stateData <- select( data, c(Alabama:Delaware, Florida:`New England`, Year ))
regionData<- select( data, c(`District of Columbia`, Mideast:`Far West`   , Year ))
UnitedData<- select( data, c(`United States`      , Year ))

# function to pool geom_line data
updateGraph<- function( item ){
  geom_line( aes_q( y = as.name( item ), color = item ))
}

# Plot all Data
gData<- ggplot( data, aes( x = Year))
gData + map( colnames( data )[ -ncol( data )], updateGraph  ) + ggtitle("All Data GDPs") + labs( y = 'GDP')

# Plot State Data
gStateData<- ggplot( stateData, aes( x = Year))
gStateData + map( colnames( stateData )[ -ncol( stateData )], updateGraph  ) + ggtitle("State GDPs") + labs( y = 'GDP')

# Plot Region Data
gRegionData<- ggplot( regionData, aes( x = Year))
gRegionData + map( colnames( regionData )[ -ncol( regionData )], updateGraph  ) + ggtitle("Regional GDPs") + labs( y = 'GDP')

# Plot United States Only Data
gUnitedData<- ggplot( UnitedData, aes( x = Year))
gUnitedData + map( colnames( UnitedData )[ -ncol( UnitedData )], updateGraph  ) + ggtitle("United States GDP") + labs( y = 'GDP')

