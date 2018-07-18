# Author: Collin Mitchell
# Date: 2018/4/3
# Purpose: To explore and play with Ramen Rater Data.

## - Libraries:
library( tidyverse )
library( leaflet )
library( rio )

## - VARIABLES
FILENAME <- 'data/ramen-rater-dataset.xlsx'

## - FUNCTIONS
importData <- import( )



##- Play
data <- import( FILENAME )%>%
          as_tibble %>%
          mutate( Review = as.numeric( `Review #`),
                  Stars = as.numeric( Stars )) %>%
          select( Review, Brand,Variety, Country, Stars )
  

names( data )

# Check out the top rated countries.
data %>%
  group_by( Country ) %>%
  summarise( meanStar = mean( Stars, na.rm = TRUE )) %>%
  arrange( desc( meanStar ))

# There is a typo in the United states column.
data %>%
  filter( Country == 'United States', Stars > 5 )
