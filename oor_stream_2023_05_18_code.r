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

# download the datasets
download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/AgePeriodCohort/data/births.txt", destfile="births.txt")
download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/AgePeriodCohort/data/US-EST00INT-ALLDATA.csv", destfile="US-EST00INT-ALLDATA.csv")

# read in the birth data
dat <- read.table("births.txt", header=TRUE)

# compute the mean age within the 45 to 54 age group in the years 1999 to 2013
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

# print the mean ages
mage

# Figure 2.11b
plot(years, mage, type="n", bty="l",
     xlab="", ylab="Avg age among non-Hisp whites 45-54")
grid()
lines(years, mage, lwd=3)

# note: this seems to assume that everybody born in a particular year survives
# until they are 45-54 years old, which seems like a debatable assumption

# mortality rates in 2013 by age (overall and separately by gender) from life
# tables as given in the code for this example on the book website
deathpr_by_age <- c(.003064, .003322, .003589, .003863, .004148, .004458, .004800, .005165, .005554, .005971)
deathpr_male   <- c(.003244, .003571, .003926, .004309, .004719, .005156, .005622, .006121, .006656, .007222)
deathpr_female <- c(.002069, .002270, .002486, .002716, .002960, .003226, .003505, .003779, .004040, .004301)

# read in the data
dat <- read.csv("US-EST00INT-ALLDATA.csv")

# NHWA_MALE NHWA_FEMALE

x <- sapply(years, function(year) {

   year <- 1989

   # calculate the age of people who were 45-54 years old in 'year' in 2000
   ages_in_2000 <- (2000 - year) + (45:54)

})

