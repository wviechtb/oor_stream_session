############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-08-22
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 10.1 - ?
#
# last updated: 2024-08-22

############################################################################

### 10.1: Adding predictors to a model

# download the dataset (need to do this once)
#download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/KidIQ/data/kidiq.csv", destfile="kidiq.csv")

# read in the data and inspect the first 6 rows
dat <- read.csv("kidiq.csv")
head(dat)

# load the rstanarm package
library(rstanarm)


res <- stan_glm(kid_score ~ mom_hs, data=kidiq)