############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-05-18
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 2.4
#
# last updated: 2023-05-18

############################################################################

### 2.4: Data and adjustment: trends in mortality rates

# download the dataset
download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/AgePeriodCohort/data/births.txt", destfile="births.txt")

# read in the data
dat <- read.table("births.txt", header=TRUE)

mage <- sapply(1989:2015, function(year) {
   # the age group we are interested in
   ages <- 54:45
   # check which birth years correspond to these ages
   ok <- dat$year %in% (year - ages)
   # compute the weight mean of the ages with weights equal to the number of
   # births corresponding to these ages
   weighted.mean(ages, dat$births[ok])
})
mage
