############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-04-11
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 6.5 - ?
#
# last updated: 2024-04-11

############################################################################

### 6.5: The paradox of regression to the mean

# download the dataset (only need to do this once)
#download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/PearsonLee/data/Heights.txt", destfile="heights.txt")

# read in the data
dat <- read.table("heights.txt", header=TRUE)


############################################################################
