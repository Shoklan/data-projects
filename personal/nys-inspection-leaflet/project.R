# Author: Collin Mitchell
# Purpose: To play with and use Leaflet

##-- Libraries
library( rvest )
library( tidyverse )
library( leaflet )
library( ggmap )


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

point <- geocode("ABBEY'S CATERING AND KIOSK LLC 7 ACORN VALLEY TRAIL")

# got address, info
xml2::as_list() %>% 
  length



# collect locations
locations <- map( .x = content,
                  .f = function( .x ){
                    .x[1]
                  }) %>% unlist

# convert to latitudes and longitudes
points <- tibble( lat = double( length( locations )), lon = double( length( locations )))
for( item in 1:length( locations )){
  coords <- NA
  
  repeat{
    Sys.sleep( 2 )
    coords <- geocode( locations[ item ])
    print( locations[ item ] )
    print( coords )
    points$lat[ item ] <-  coords$lat
    points$lon[ item ] <- coords$lon
    if( !is.na( sum( coords ))) break
    Sys.sleep(10)
  } # END repeat
}


# draw the map  
leaflet() %>%
  addTiles() %>%
  addMarkers( lng = points$lon, lat = point$lat)
