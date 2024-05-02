############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-05-02
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 8.1 - ?
#
# last updated: 2024-05-02

############################################################################

# for the survey about topics for future streams (alternating with the coverage
# of the present book), go here: https://forms.gle/VEEKNRvquRCGKjb78

############################################################################

# - version R 4.4-0 released a week ago
# - security vulnerability in older version of R
#   - https://hiddenlayer.com/research/r-bitrary-code-execution/
#   - https://www.kb.cert.org/vuls/id/238194
#   - https://nvd.nist.gov/vuln/detail/CVE-2024-27322
#   - https://stat.ethz.ch/pipermail/r-help/2024-May/479281.html
# - sort_by() function

############################################################################

### 8.1: Least squares, maximum likelihood, and Bayesian inference

## Least squares

# simulate some data based on a simple regression model
set.seed(1234)
n <- 50
x <- runif(n, 0, 10)
y <- 2 + 0.5 * x + rnorm(n, mean=0, sd=1)

# plot the data
plot(x, y, pch=21, bg="gray")

# add the true regression line to the plot
abline(a=2, b=0.5, lwd=3)

# since we know the true regression line here, we can compute the errors
y - (2 + 0.5 * x)

# download the dataset (only need to do this once)
#download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/ElectionsEconomy/data/hibbs.dat", destfile="hibbs.dat")

# read in the data and inspect it
dat <- read.table("hibbs.dat", header=TRUE)
dat

############################################################################
