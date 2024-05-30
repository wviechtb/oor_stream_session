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

# create the dataset for the example and examine it
dat <- structure(list(x = c(-2, -1, 0, 1, 2), y = c(50, 44, 50, 47, 56)),
                 row.names = c(NA, -5L), class = "data.frame")
dat

# percentage of girls of parents in the lower attactiveness categories versus
# the highest attractiveness category
p1 <- mean(dat$y[1:4])
p2 <- dat$y[5]
p1
p2

# difference between the two percentages
p2 - p1

# about 90% of the 3000 participants of the survey fell into the first four
# categories and 10% in the highest category
n1 <- 90/100 * 3000
n2 <- 10/100 * 3000

# standard error of the difference between the two percentages
se <- sqrt(p1 * (100 - p1) / n1 + p2 * (100 - p2) / n2)
se

# mean and SE for the prior (for the difference between the two percentages)
theta_hat_prior <- 0
se_prior <- 0.5

# combine the prior with the data using equation (9.3) and (9.4)
theta_hat_data <- p2 - p1
se_data <- se
theta_hat_bayes <- (theta_hat_prior/se_prior^2 + theta_hat_data/se_data^2) / (1/se_prior^2 + 1/se_data^2)
se_bayes <- sqrt(1/(1/se_prior^2 + 1/se_data^2))
theta_hat_bayes
se_bayes
