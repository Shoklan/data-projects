#   Author: Collin Mitchell
#     Date: 2017/1/6
# fin-Date: 2017/1/6
#     Role: Cleaning Poll Data

library( rio )
library( tidyverse )
library( data.table )


path<- file.path( 'Code', 'data-projects', 'dataworld', 'teen-education-poll')
setwd( path )

# Online URL source: https://data.world/afterschool/teen-education-poll/file/Education%20Poll.pdf

# first we need to correct the data from PDF

# data recreation:
pollingData <- data.table(
  State = c('Delaware',
            'New Mexico',
            'North Carolina',
            'Nevada',
            'Ohio',
            'Arizona',
            'Mississipi',
            'South Carolina',
            'Indiana',
            'Maryland',
            'South Dakota',
            'Alaska',
            'Oklahoma',
            'Tennessee',
            'Hawaii',
            'California',
            'Florida',
            'Utah',
            'Texas',
            'Kentucky',
            'Oregon',
            'Missouri',
            'New Hampshire',
            'Louisiana',
            'Colorado',
            'Pennsylvania',
            'Arkansas',
            'Virginia',
            'Nebraska',
            'Wisconsin',
            'Kansas',
            'Michigan',
            'Montana',
            'Idaho',
            'Washington',
            'Rhode Island',
            'Georgia',
            'Illinois',
            'New York',
            'Connecticut',
            'Minnesota',
            'Alabama',
            'Wyoming',
            'Iowa',
            'New Jersey',
            'Massachusetts',
            'Vermont',
            'Maine',
            'West Virginia',
            'North Dakota'),
  Yes = c(228, 108, 724, 213, 988,
          332, 242, 235, 595, 512,
          116, 57, 409, 402, 44,
          1849, 982, 234, 1686, 337,
          658, 601, 273, 275, 453,
          1619, 385, 867, 170, 681,
          394, 1320, 167, 193, 1006,
          91, 290, 1292, 3163, 665,
          602, 328, 105, 445, 1293,
          1088, 119, 382, 257, 59),
  No = c(111, 63, 429, 127, 607,
         204, 151, 148, 376, 326,
         74, 37, 266, 264, 29,
         1220, 649, 155, 1125, 253,
         446, 409, 187, 189, 316,
         1137, 271, 611, 122, 491,
         287, 972, 125, 145, 758,
         69, 221, 985, 2433, 512,
         466, 255, 82, 357, 1039, 
         892, 108, 360, 247, 58)
)

# How many per state? 
pollingData[, Total := Yes + No]

# How many yes/no?
pollingData[, PercentYes := Yes/Total ]
pollingData[, PercentNo := No/Total ]

# correct percentage to be 2 digits.
pollingData[, PercentYes := round( PercentYes, 2)]
pollingData[, PercentNo  := round( PercentNo , 2)]

# ensure that data is representative
pollingData[, .( sum( No  ) / ( sum(Yes) + sum(No) )) ]
pollingData[, .( sum( Yes ) / ( sum(Yes) + sum(No) )) ]

goodEducation<- data.table( 
  Content = c("My Teachers",
              "Other",
              "Motivated and Work Hard",
              "Size of Classes",
              "Books and Resources"),
  YesCount = c(18358, 12432, 11918, 3969, 3790),
  NoCount  = c(17065, 9130, 8715, 2154, 2544)
)


export(pollingData, file.path( 'data', 'pollingData.csv') )
export(goodEducation, file.path( 'data', 'goodEducation.csv') )