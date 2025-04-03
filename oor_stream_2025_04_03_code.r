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


library(tinyplot)
tinytheme("ridge2")
plt(post, data=dat, type="ridge", legend=FALSE)

plt(Month ~ Temp, data=dat, type="ridge", gradient=TRUE, scale=0.8,
    palette=hcl.colors(n=100, palette="temps", rev=TRUE))



library(ggplot2)
library(bayesplot)

p0 <- mcmc_areas(as.matrix(res0), pars=vars(-'(Intercept)',-sigma),
                 prob_outer=0.95, area_method = "scaled height")
p0 <- p0 + scale_y_discrete(limits = rev(levels(p0$data$parameter)))
p0

