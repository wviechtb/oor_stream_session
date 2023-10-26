############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-10-26
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 4.3 - ?
#
# last updated: 2023-10-26

############################################################################

### 4.3: Bias and unmodeled uncertainty

# Bias in estimation

# according to recent data, men in the US watch around 3 hours, women around
# 2.5 hours per day of telvision (and let's assume a standard deviation of 0.5)
hrs.m <- rnorm(50, mean=3.0, sd=0.5)
hrs.w <- rnorm(50, mean=2.5, sd=0.5)
