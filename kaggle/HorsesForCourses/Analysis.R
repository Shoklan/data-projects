# Questions?
#- Predict Winner?

# Notes:
#- weathers.csv id 4 missing data.

library( data.table ) #fread
library( ggplot2 )
library( dplyr )
library( SOAR )

# data imports
dataNamesVector <- c('conditions', 'forms', 'horses', 'markets', 'odds', 'riders', 'runners', 'weathers')
for( item in dataNamesVector ){ assign( item, fread( file.path( 'data', paste0( item, '.csv' ) ))) }


# convert over weathers and then delete csv
for(i in 1:nrow( weathers )){ markets$weather_id <- gsub( weathers[i,1, with = F], weathers[i,2, with = F], markets$weather_id) }
rm( weathers)

# convert over conditions and then delete csv
for( i in 1:nrow( conditions)){ markets$condition_id <- gsub( conditions[i,1, with = F], conditions[i,2, with = F], markets$condition_id) }
markets$condition_id
rm( conditions )



##-----------------------------------

# merge riders and runners
setkey(riders, id)
setkey(runners, rider_id)
runners<- runners[ riders ]

# delete
rm( riders )

setkey( forms, horse_id)
setkey( horses, id)
forms<- forms[ horses]
rm( horses )



setkey( runners, market_id )
setkey( markets, id)
markets<- markets[ runners]
rm( runners )

odds$runner_id
setkey( odds, runner_id)ll
setkey( markets, i.id)
markets<- markets[ odds ]
rm( odds )
colnames( markets )

rm( temp )

setkey( forms, market_id)
setkey( markets, id)
dataset<- forms[ markets]
nrow( markets )
