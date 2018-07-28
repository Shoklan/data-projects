# Author: Collin Mitchell
# Purpose: To play with and use Leaflet

##-- Libraries
library( rvest )
library( tidyverse )
library( leaflet )
library( ggmap )
library( magrittr )


##-- Variables:
targetUrl <- "https://www.newyorkupstate.com/restaurants/2018/07/monroe_county_restaurant_inspections_from_january_to_june_2018.html"



##-- Functions:

# Return a session object
getSession<- function( urlTarget ){
  sessionObject <- html_session( urlTarget )
  sessionObject
}


##-- Main


##-- Play

session <- getSession( targetUrl )
content <- session %>%
  read_html( targetUrl ) %>%
  html_nodes( ".entry-content > p") %>%
  # head %>%
  html_text() %>%
  map( str_split, pattern = ",", n = 2) %>%
  map( unlist )


# collect locations
locations <- map( .x = content,
                  .f = function( .x ){
                    .x[1]
                  }) %>% unlist

# let google know this is rochester
locations <- map(locations, str_c, ", ROCHESTER") %>% unlist

# convert to latitudes and longitudes
points <- tibble( lat = double( length( locations )), lon = double( length( locations )))
for( item in 1:length( locations )){
  coords <- NA
  timer <- 0
  
  repeat{
    Sys.sleep( 2 )
    coords <- geocode( locations[ item ])
    print( locations[ item ] )
    print( coords )
    
    if( timer == 5 || !is.na( sum( as.numeric( coords )))){
      points$lat[ item ] <- coords$lat
      points$lon[ item ] <- coords$lon
      break
    }
    Sys.sleep(10)
    timer <- timer +1
  } # END repeat
}


points %<>% filter( !is.na( lat ))


# draw the map  
leaflet() %>%
  addTiles() %>%
  addMarkers( lng = points$lon, lat = point$lat)
