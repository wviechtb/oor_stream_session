############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-12-12
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 11.7
#
# last updated: 2024-12-12

############################################################################

### 11.7: External validation: checking fitted model on new data

# load the rstanarm package
library(rstanarm)

# download the dataset if it doesn't already exist
if (!file.exists("kidiq.csv")) download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/KidIQ/data/kidiq.csv", destfile="kidiq.csv")

# read in the data and inspect the first 6 rows
dat <- read.csv("kidiq.csv")
head(dat)

# the dataset doesn't actually include information about the age of the
# children, so we cannot replicate exactly what is described in the book;
# however, let's pretend that the first 98 rows of the dataset contains the
# data of the older children, so we fit the model based on this subset
res1 <- stan_glm(kid_score ~ mom_iq + mom_hs, data=dat, refresh=0, subset=1:98)


