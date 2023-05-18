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
download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/AgePeriodCohort/data/white_nonhisp_death_rates_from_1999_to_2013.txt", destfile="white_nonhisp_death_rates_from_1999_to_2013.txt")

# read in the mortality data
dat <- read.table("white_nonhisp_death_rates_from_1999_to_2013.txt", header=TRUE)

# compute the total number of deaths and the population size in each year
# between 1999 and 2013 for people between 45 and 54 years of age
dat <- aggregate(dat[c("Deaths","Population")], list(Year = dat$Year), sum)

# compute the raw death rate for each year
dat$Rates <- with(dat, Deaths / Population)

# Figure 2.11a
plot(dat$Year, dat$Rates, type="n", bty="l",
     xlab="", ylab="Death rate among non-Hisp whites 45-54")
grid()
lines(dat$Year, dat$Rates, lwd=3)

# make a copy of dat for Figure 2.11c
sav <- dat

# read in the mortality data again
dat <- read.table("white_nonhisp_death_rates_from_1999_to_2013.txt", header=TRUE)

# compute the mean age within the 45 to 54 age group in the years 1999 to 2013
years <- 1999:2013
mage <- sapply(years, function(year) {
   weighted.mean(45:54, dat[dat$Year == year, "Population"])
})

# Figure 2.11b
plot(years, mage, type="n", bty="l",
     xlab="", ylab="Avg age among non-Hisp whites 45-54")
grid()
lines(years, mage, lwd=3)

# read in the mortality data again
dat <- read.table("white_nonhisp_death_rates_from_1999_to_2013.txt", header=TRUE)

# extract the 2013 mortality rates
rates2013 <- with(dat[dat$Year == 2013,], Deaths / Population)

rates.adj <- sapply(years, function(year) {
   weighted.mean(rates2013, dat$Population[dat$Year == year])
})

# Figure 2.11c
plot(sav$Year, sav$Rates, type="n", bty="l",
     xlab="", ylab="Death rate among non-Hisp whites 45-54")
grid()
lines(sav$Year, sav$Rates, lwd=3)
lines(years, rates.adj, lwd=3)

