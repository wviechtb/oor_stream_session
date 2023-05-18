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

# compute the mean age within the 45 to 54 age group in the years 1989 to 2015
years <- 1999:2013
mage <- sapply(years, function(year) {
   # the age group we are interested in
   ages <- 54:45
   # check which birth years correspond to these ages
   ok <- dat$year %in% (year - ages)
   # compute the weight mean of the ages with weights equal to the number of
   # births corresponding to these ages (note: we need to specify the ages
   # above from oldest to youngest, so that the birth numbers correspond to
   # the ages)
   weighted.mean(ages, dat$births[ok])
})

mage

# Figure 2.11(c)
plot(years, mage, type="n", bty="l",
     xlab="", ylab="Avg age among non-Hisp whites 45-54")
grid()
lines(years, mage, lwd=3)
