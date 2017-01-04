#   Author: Collin Mitchell
#     Date: 2017/1/2
# fin-Date: ###
#     Role: Exploring Crime Data.


# Inquiries. #
# 


# Packages
library( rio )
library( tidyverse )
library( data.table )

# Correct path
path<- file.path( 'Code', 'data-projects', 'dataworld', 'chicago-crime-2016')
setwd( path )

# Read data
data<- fread( file.path('data', 'Crimes_2016.csv' ) )

colnames( data )

# what type?
map_chr( colnames( data ), typeof)
# all char?


# lot of fixing to do.


# Date > POSIX



# arrest > format
# domestic > format


# community? > delete?
# FBI code? > ?


# Updated on > POSIX

# location > DEL

colnames( data )
NUMERIC_ALTER <- c( 1,5,11,12,15,16,17,18,20,21 )

