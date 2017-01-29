#   Author: Collin Mitchell
#     Date: 2017/1/29
# fin-Date: ###
#     Role: Creating data to work with.

figure1<- data.table(
  month      = c( "June", "July", "August", "September", "October", "November", "December", "January", "February", "March"),
  year       = c(   2015,   2015,     2015,        2015,      2015,       2015,       2015,      2016,       2016,    2016),
  identified = c(     87,    154,      138,         144,       119,        132,        156,       128,        141,     149))

write.csv(figure1, paste0(filepath, "/figure1.csv") )


figure3<- data.table( 
  category   = c( "1", "2", "3", "4-10", "11-20", "21-100", "101+"),
  identified = c(   18, 11,   9,     36,       15,       9,      2))

write.csv(figure3, paste0(filepath, "/figure3.csv") )

table4<- data.table(
  State = c("Alabama", "Arkansas", "Arizona", "California", "Colorado", "Connecticut", "Florida", "Georgia", "Hawaii", "Iowa", "Idaho",
            "Illinois", "Indiana", "Kansas", "Kentucky", "Louisiana", "Massachusetts", "Maryland", "Maine", "Michigan", "Minnesota", "Missouri",
            "Mississipi", "Montana", "North Carolina", "Nebraska", "New Hampshire", "New Jersey", "New Mexico", "Nevada", "New York", "Ohio",
            "Oklahoma", "Oregon", "Pennsylvania", "South Carolina", "Tennessee", "Texas", "Utah", "Virginia", "Washington", "Wisconsin", "West Virginia",
            "Wyoming"),
  Deaths                 = c( 8,  1, 10, 64, 7, 2, 25, 14, 2, 2, 2, 11, 10,  6, 11,  7, 6, 4, 5, 5, 3, 12, 3,  1, 10, 1, 1, 8, 8, 10, 11, 16, 10,  7, 13, 7,  8, 58, 6, 7, 11, 5, 4, 3 ),
  MediaIdentifiedDeaths  = c( 8,  1,  9, 60, 7, 2, 25, 12, 2, 2, 2, 8, 10,  6, 10,  7,  5, 3, 1, 5, 3, 6,  3, 1, 10,  1, 1, 8, 7, 9, 11, 12, 10,  7, 12,  7, 8, 43,  6, 7, 8,  5, 4, 3),
  AgencyIdentifiedDeaths = c( 0,  0,  1,  4, 0, 0,  0,  2, 0, 0,0,   3,  0,  0,  1,  0, 1, 1, 4, 0, 0,  6, 0,  0,  0, 0, 0, 0,  1,  1, 0,  4,  0,  0,  1, 0,  0, 15, 0, 0,  3, 0, 0, 0 )
)

write.csv(table4, paste0(filepath, "/table4.csv") )