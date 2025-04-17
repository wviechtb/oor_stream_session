############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-04-17
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 13.1 - ?
#
# last updated: 2025-04-17

############################################################################

# load the rstanarm package
library(rstanarm)

############################################################################

### 13.1: Logistic regression with a single predictor

# make copies of the qlogis and plogis functions with easier to remember names
logit <- qlogis
invlogit <- plogis

## Example: modeling political preference given income

# download the dataset if it doesn't already exist
if (!file.exists("nes.txt")) download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/refs/heads/master/NES/data/nes.txt", destfile="nes.txt")

# read in the dataset
dat <- read.csv("student-merged.csv")

# inspect the first six rows of the dataset
head(dat)

https://github.com/avehtari/ROS-Examples/raw/refs/heads/master/NES/data/nes.txt