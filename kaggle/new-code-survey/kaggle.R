#
#
# 2016-6-15
# tried using fread, but too many warnings


library(data.table)
library(ggplot2)
library(dplyr)

path<- file.path( 'Code', 'data-projects', 'kaggle', 'new-code-survey')
setwd( path )

file<- list.files(recursive = T)
data<- fread(file)

# info

dim( data )
colnames( data )

# Gender vs Age?

data$Gender<- as.factor( data$Gender )
levels( data $ Gender )

# I needed 
meanAge <- data.frame( meanAge =rep(mean(data$Age, na.rm = T), length( data$Age ))  )

g<- ggplot(data, aes(Gender, Age)) + geom_boxplot()
g<- g + geom_hline(data = meanAge, yintercept = metrics$meanAge[1], color = 'red', size = 1)
g<- g + ggtitle('Age Vs Gender')
g

# Interesting to note that the average age is above the mean of all ages.

naIndexesAge<- which( is.na(data$Age) )
summary(data$Age )[ 7 ] == length(naIndexesAge)

naIndexesGender<- which( is.na(data$Gender) )
summary( data$Gender )[ 6 ] == length(naIndexesGender )

missingInfoIntersect<- intersect(naIndexesAge, naIndexesGender)

length(missingInfoIntersect)

# a lot of people that didn't include their gender also didn't include their age;
# these people are probably very privacy oriented.
# investigate how much info they gave away after

?select

alteredData<- slice(data, -naIndexesAge)
alteredData<- slice(data, -naIndexesGender)

summary(alteredData$Gender)
summary(alteredData$Age)

# still getting some NAs?

alteredNaIndexes<- which( is.na(alteredData$Age) )

alteredData<- slice(alteredData, -alteredNaIndexes)

summary(alteredData$Gender)
summary(alteredData$Age)

# NOW we're getting no NA's, so lets try and plot this.
meanAge <- data.frame( meanAge =rep(mean(alteredData$Age), length( alteredData$Age ))  )

g<- ggplot(alteredData, aes(Gender, Age)) + geom_boxplot()
g<- g + geom_hline(data = meanAge, yintercept = meanAge$meanAge[1], color = 'red', size = 1)
g<- g + ggtitle('Age Vs Gender')
g

# even after adjusting for the missing values, there outlies are STILL pulling the mean up high.
# Data is interesting, but no real leads here.

# Lets go after the privacy line of thought

# Let's not forget that something with which is not quite right.
ageNAs<- c(alteredNaIndexes, naIndexesAge)
genderNAs<- naIndexesGender
# do I even need these?

# Remember that there are ~1800 missing values
length(missingInfoIntersect)

naSampleIndexes<- sample( missingInfoIntersect, 1500)
nonNaSampleIndexes<- sample( 1:nrow(alteredData), 1500)

# remember that technically there is no distinction here since indexes are all coming from same place.

meanNaSampleMissing<-    mean(sapply(1:length( naSampleIndexes ), function(i) { sum(is.na(data[ naSampleIndexes[ i ] ]))}))
meanNonNaSampleMissing<- mean(sapply(1:length( naSampleIndexes ), function(i) { sum(is.na(alteredData[ nonNaSampleIndexes[ i ] ]))}))

# Users that do not include their gender answer about 22 less questions

meanNonNaSampleMissing / meanNaSampleMissing
# users that don't include gender give away about 25% less information than their cohorts.

# fluke? CRANK IT TO 1800

naSampleIndexes<- sample( missingInfoIntersect, 1800)
nonNaSampleIndexes<- sample( 1:nrow(alteredData), 1800)

# remember that technically there is no distinction here since indexes are all coming from same place.

meanNaSampleMissing<-    mean(sapply(1:length( naSampleIndexes ), function(i) { sum(is.na(data[ naSampleIndexes[ i ] ]))}))
meanNonNaSampleMissing<- mean(sapply(1:length( naSampleIndexes ), function(i) { sum(is.na(alteredData[ nonNaSampleIndexes[ i ] ]))}))

meanNonNaSampleMissing / meanNaSampleMissing

# Still getting about 25% less data.

