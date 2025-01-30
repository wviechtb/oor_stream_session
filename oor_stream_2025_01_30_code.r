############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-01-30
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 12.1 - ?
#
# last updated: 2025-01-30

############################################################################

# load the rstanarm package
library(rstanarm)

### 12.1: Linear transformations

# download the dataset (only need to do this once)
if (!file.exists("earnings.csv")) download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/Earnings/data/earnings.csv", destfile="earnings.csv")

# read in the dataset
dat <- read.csv("earnings.csv")

# inspect the first six rows of the dataset
head(dat)

# fit a model predicting earnings from height
res <- stan_glm(earn ~ height, data=dat, refresh=0, seed=7783)
summary(res)
