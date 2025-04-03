############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-04-03
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 12.7 - ?
#
# last updated: 2025-04-03

############################################################################

# load the rstanarm package
library(rstanarm)

############################################################################

### 12.7: Models for regression coefficients

# download the dataset if it doesn't already exist
if (!file.exists("student-merged.csv")) download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/refs/heads/master/Student/data/student-merged.csv", destfile="student-merged.csv")

# read in the dataset
dat <- read.csv("student-merged.csv")

# inspect the first six rows of the dataset
head(dat)

# set up a vector with the names of the predictor variables
predictors <- c("school","sex","age","address","famsize","Pstatus","Medu",
                "Fedu", "traveltime","studytime","failures","schoolsup",
                "famsup", "paid","activities","nursery","higher","internet",
                "romantic","famrel","freetime","goout","Dalc","Walc","health",
                "absences")

# select rows where the final-year mathematics grade (G3mat) is > 0 and only
# select this variable plus the predictors
dat <- subset(dat, subset=G3mat>0, select=c("G3mat",predictors))
head(dat)

# predict G3mat from all other variables in the dataset
res0 <- stan_glm(G3mat ~ ., data=dat, refresh=0)
print(res0, digits=2)

# extract the posterior samples
post <- as.data.frame(res0)
post <- post[-c(1,ncol(post))]

# Figure 12.10a: Plot of kernel density estimates of the posterior
# distributions of the coefficients for the predictors

par(mar=c(4,8,2,2), las=1)
plot(NA, xlim=range(post), ylim=c(1,ncol(post)), yaxt="n", bty="l",
     xlab="", ylab="")
abline(v=0, lty="dotted")

for (i in 1:ncol(post)) {
   tmp <- density(post[,i])
   tmp$y <- tmp$y / max(tmp$y) * 0.9
   cutoffs <- quantile(post[,i], prob=c(0.01,0.99))
   tmp$y <- tmp$y[tmp$x > cutoffs[1] & tmp$x < cutoffs[2]]
   tmp$x <- tmp$x[tmp$x > cutoffs[1] & tmp$x < cutoffs[2]]
   lines(tmp$x, tmp$y+ncol(post)+1-i)
   lines(tmp$x, rep(ncol(post)+1-i, length(tmp$x)))
   segments(mean(tmp$x), ncol(post)+1-i, mean(tmp$x), ncol(post)+1-i+0.9)
}

axis(side=2, at=(ncol(post)):1, label=colnames(post))

# make a copy of the dataset and standardize all of the predictors
dat2 <- dat
dat2[,predictors] <- scale(dat2[,predictors])

# fit the model with all predictors standardized
res1 <- stan_glm(G3mat ~ ., data=dat2, refresh=0)

# extract the posterior samples
post <- as.data.frame(res1)
post <- post[-c(1,ncol(post))]

# Figure 12.10b: Like the previous figure, but with standardized predictors

par(mar=c(4,8,2,2), las=1)
plot(NA, xlim=range(post), ylim=c(1,ncol(post)), yaxt="n", bty="l",
     xlab="", ylab="")
abline(v=0, lty="dotted")

for (i in 1:ncol(post)) {
   tmp <- density(post[,i])
   tmp$y <- tmp$y / max(tmp$y) * 0.9
   cutoffs <- quantile(post[,i], prob=c(0.01,0.99))
   tmp$y <- tmp$y[tmp$x > cutoffs[1] & tmp$x < cutoffs[2]]
   tmp$x <- tmp$x[tmp$x > cutoffs[1] & tmp$x < cutoffs[2]]
   lines(tmp$x, tmp$y+ncol(post)+1-i)
   lines(tmp$x, rep(ncol(post)+1-i, length(tmp$x)))
   segments(mean(tmp$x), ncol(post)+1-i, mean(tmp$x), ncol(post)+1-i+0.9)
}

axis(side=2, at=(ncol(post)):1, label=colnames(post))

# close the plot
graphics.off()

# compute the Bayesian R^2 (median of the posterior R^2 distribution)
postR2 <- bayes_R2(res1)
round(median(postR2), digits=2)

# compute the leave-one-out R^2
round(median(loo_R2(res1)), digits=2)

# compute LOO log score
loo1 <- loo(res1)
loo1

# note: the model is predicting the mean of the outcome variable
# E[y] = b1*z1 + b2*z2 + ... + bp*zp
#
# so the variance in E[y] is given by this:
# Var(E[y]) = Var(b1*z1 + b2*z2 + ... + bp*zp)
#
# now assuming independence between the predictors and the priors for the
# regression coefficients, we can rewrite this as:
#           = Var(b1*z1) + Var(b2*z2) + ... + Var(bp*zp)
#
# since we assume priors with a mean of 0 and the predictors are also z-scored
# and hence have a mean of 0, we can rewrite this as:
#           = Var(b1)*Var(z1) + ... + Var(bp)*Var(zp)
#
# and since the predictors have a variance of 1, we can rewrite this as:
#           = Var(b1) + ... + Var(bp)
#
# and since we assume a standard deviation of 2.5 for the prior distributions,
# we can simply this to:
#           = 2.5^2 * p
#
# and so SD(E[y])  = 2.5 * sqrt(p)

# so in the present case, the standard deviation of the predicted mean based
# on the prior distributions is approximately
musd <- round(2.5 * sqrt(ncol(dat2)-1), digits=2)
musd

# the default prior for sigma is an exponential distribution, scaled to have
# mean equal to the standard deviation of the outcome, which in this case is
# approximately 3.3
esd <- round(sd(dat$G3mat), digits=2)
esd

# note: R^2 is roughly how much of the total variance (which consists of
# variance in the predicted means plus the error variance) is due to the
# variance in the predicted means
musd^2 / (musd^2 + esd^2)

# to generate a whole prior distribution for R^2, we simulate many times
# sigma2 and beta values from their respective prior distributions and repeat
# the compuation of R^2 for each of the simulated values
priorR2 <- replicate(4000, {
   sigma2 <- rexp(1, rate=0.3)^2
   muvar  <- var(c(as.matrix(dat2[,predictors]) %*% rnorm(26, mean=0, sd=2.5)))
   muvar / (muvar + sigma2)
})

# Figure 12.11 (top left): plot the prior distribution for R^2
hist(priorR2, breaks=seq(0,1,by=.01), main="Prior Distribution of R^2", xlab="")

# Figure 12.11 (bottom left): plot the posterior distribution for R^2
hist(postR2, breaks=seq(0,1,by=.01), main="Posterior Distribution of R^2", xlab="")

# now say we assume that the best we might be able to do is to account for
# about 30% of the variance (so R^2 =~ 0.30); then what would muvar have to be
# to get such an R^2?
# muvar / (muvar + 3.3^2) = 0.3
# (muvar + 3.3^2) / muvar = 1 / 0.3
# 1 + 3.3^2 / muvar = 1 / 0.3
# 3.3^2 / muvar = (1 - 0.3) / 0.3
# muvar / 3.3^2 = 0.3 / (1 - 0.3)
# muvar = 0.3 / (1 - 0.3) * 3.3^2

muvar / (muvar + 3.3^2)



mean(ppR2)


pred <- posterior_linpred(res1)
pred <- apply(pred, 2, mean)
sd(pred)

var(pred) / (var(pred) + 2.885056^2)


x1 <- rnorm(100000, mean=100, sd=15)
x2 <- rnorm(100000, mean=10, sd=1)
z1 <- c(scale(x1)) * 2.5
z2 <- c(scale(x2))
var(z1*z2)
var(z1)*var(z2)
