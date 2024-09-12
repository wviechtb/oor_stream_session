############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-09-12
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 10.6 - ?
#
# last updated: 2024-09-12

############################################################################

### 10.6: Example: uncertainty in predicting congressional elections

# download the data for the example
if (!file.exists("congress.csv")) download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/Congress/data/congress.csv", destfile="congress.csv")

# read in the data and inspect the first 6 rows
dat <- read.csv("congress.csv")
head(dat)

# Figure 10.5: Proportion of the vote share for the Democratic party in the
# 435 congressional districts in 1988
hist(dat$v88, breaks=seq(0,1,by=.05))
