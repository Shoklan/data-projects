library(data.table)
library(ggplot2)
library(stringr)
library(dplyr)

# Questions:
# Worst district/yr?


data<- fread("crime_homicide_subset.csv")


##### EXPLORE #######

colnames( data )
#> colnames( data )
#[1] "year"                 "mont"                 "week"                 "hour"                 "start"                "day"                 
#[7] "REPORT_DAT"           "SHIFT"                "OFFENSE"              "METHOD"               "BLOCK"                "DISTRICT"            
#[13] "PSA"                  "WARD"                 "ANC"                  "NEIGHBORHOOD_CLUSTER" "BLOCK_GROUP"          "CENSUS_TRACT"        
#[19] "VOTING_PRECINCT"      "CCN"                  "XBLOCK"               "YBLOCK"               "START_DATE"           "END_DATE"            
#[25] "lat"                  "long"                 "accuracy"

# NAs?
for(i in 1:ncol(data)){ print( paste(colnames(data)[i], ' ', sum(is.na(data[,i, with = F]))))}
# Some, but not in anything I care about for this analysis


# unique data?
for(i in 1:ncol(data)){ print(unique(data[,i, with = F]))}

unique(data$year)
unique(data$mont)
unique(data$day)  # resolved

# get the real days and replace
data$day <-substring(str_extract(data$REPORT_DAT, '/[0-9]*'), 2)
unique(data$hour) # military time

# Don't need:
unique(data$week)


data$REPORT_DAT<- as.POSIXct(data$REPORT_DAT, format = '%m/%d/%Y %H:%M') # don't need to adjust timezone since DC is EST



data[, .(year := NULL,
         week := NULL,
         day  := NULL,
         hour := NULL)]



yearScale <- data.table( year = c(2011, 2012, 2013, 2014, 2015, 2016), count = as.numeric(0))

yearScale$count[1] <- nrow( filter(data, REPORT_DAT < as.POSIXct('2011/6/1')) )
yearScale$count[2] <- nrow( filter(data, REPORT_DAT < as.POSIXct('2012/6/1') &  REPORT_DAT > as.POSIXct('2012/1/1')) )
yearScale$count[3] <- nrow( filter(data, REPORT_DAT < as.POSIXct('2013/6/1') &  REPORT_DAT >= as.POSIXct('2013/1/1')) )
yearScale$count[4] <- nrow( filter(data, REPORT_DAT < as.POSIXct('2014/6/1') &  REPORT_DAT >= as.POSIXct('2014/1/1')) )
yearScale$count[5] <- nrow( filter(data, REPORT_DAT < as.POSIXct('2015/6/1') &  REPORT_DAT >= as.POSIXct('2015/1/1')) )
yearScale$count[6] <- nrow( filter(data, REPORT_DAT > as.POSIXct('2016/1/1') &  REPORT_DAT >= as.POSIXct('2016/1/1')) )

g<- ggplot(yearScale, aes( x = year, y = count)) + geom_point(size = 4) +  geom_smooth(se = TRUE)
g <- g + ggtitle('Crime Within First 5 Months') + labs(x = 'Year of Crimes', y = 'Count of Crime')
print( g )

g<- ggplot(data, aes( factor(year), fill = factor(OFFENSE) )) + geom_bar(position = 'dodge')
g <- g + guides(fill = guide_legend(title = 'Type of Crime')) + labs( x = 'Year', y = 'Frequency') + ggtitle('Crime Type By Year')
print( g )


################

qplot(data$lat, data$long)
# what is that dot so far away?

outliers<- filter(data, lat< 38.4)
outliers
#################


unique(data$DISTRICT)

#  remember that data for 2016 is not completed
districtSubset<- data[, .(count = .N), by = .(year, DISTRICT)]
g<- ggplot(districtSubset, aes(x = year, y = count, group = DISTRICT, color = DISTRICT)) + geom_line() + scale_color_gradient(low = 'yellow', high = 'red')
g <- g + ggtitle("Danger of Districts") + labs(x = 'Year', y = 'Offenses') + theme_dark()
print( g )