############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-05-30
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 9.4 - ?
#
# last updated: 2024-05-30

############################################################################

### 9.4: Example of Bayesian inference: beauty and sex ratio

# download the dataset for the example (run once)
#download.file("https://github.com/avehtari/ROS-Examples/raw/master/SexRatio/data/sexratio.rda", destfile="sexratio.rda")

# read in the data
dat <- readRDS("sexratio.rda")
