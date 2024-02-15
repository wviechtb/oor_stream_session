############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-02-15
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 4.6 - 4.8
#
# last updated: 2024-02-15

############################################################################

### 4.6: Example of hypothesis testing: 55,000 residents need your help!

# download the dataset (only need to do this once)
#download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/Coop/data/Riverbay.csv", destfile="riverbay.csv")

# read in the data and inspect the dataset
dat <- read.csv("riverbay.csv", header=FALSE)
dat

# remove the first column
dat <- dat[-1]

# give proper names to the variables in the dataset
names(dat) <- c(paste0("tally", 1:6), "candidate")

# cumulative number of voters at the tallies (note: each voter could vote for
# up to 6 candidates, so the total counts at each tally can be as high as 6
# times the number of voters)
cumulvoters <- c(600, 1200, 2444, 3444, 4444, 5553)

# number of voters at each tally
voters <- cumulvoters - c(0,cumulvoters[1:5])
voters

# proportion of votes received at the very end
dat$propend <- dat$tally6 / cumulvoters[6]

# matrix with votes received at each tally for the 27 candidates
votes <- (dat[,1:6] - cbind(0,dat[,1:5]))

# sort the dataset by proportion of votes received (in decreasing order, so
# the first row is the person who received the most votes)
dat <- dat[order(dat$propend, decreasing=TRUE),]

# reset the row names to increasing integers
rownames(dat) <- NULL

# inspect the dataset one more time
dat

# Figure 4.5

par(mfrow=c(2,4), mar=c(3,4,2,2))

for (i in 1:8) {

   plot(cumulvoters, dat[i,1:6]/cumulvoters,
        type="o", pch=21, bg="gray", xlim=c(0,6000), ylim=c(0,0.6),
        xlab="", ylab="", main=dat$candidate[i])

}

# Figure 4.6

par(mfrow=c(2,4), mar=c(3,4,2,2))

for (i in 1:8) {

   plot(cumulvoters, votes[i,] / voters,
        type="o", pch=21, bg="gray", xlim=c(0,6000), ylim=c(0,0.6),
        xlab="", ylab="", main=dat$candidate[i])

}

# matrix with the proportions of votes received at each tally
propmat <- t(t(votes) / voters)
propmat

# compute the standard deviation of those proportions within each row (these
# are the T_i values on page 64)
sds.obs <- apply(propmat, 1, sd)
sds.obs

# matrix with the estimated sampling variances (i.e., the square of the
# standard errors) for each of the 27*6 proportions
varmat <- t(sapply(dat$propend, function(p) p * (1-p) / voters))
varmat

# compute the average of these sampling variances across rows and take the
# square root thereof (these are the T_i^theory values as on page 64)
sds.theory <- sqrt(apply(varmat, 1, mean))

# Figure 4.7 (except that we directly put the proportions on the x-axis and
# not the total number of votes, since this is clearer)

plot(dat$propend, sds.obs, pch=21, xlab="proportion # of votes for the candidate",
     ylab="sd of separate vote proportions")
points(dat$propend, sds.theory, pch=19)

# run a chi-square test of independence of the votes received / not received
# by each candidate at the 6 time points
pvals <- apply(votes, 1, function(x) chisq.test(rbind(x, voters - x))$p.value)

# proportion of p-values below 0.1 and above 0.9
mean(pvals < 0.1)
mean(pvals > 0.9)

# we can check if the p-values come from a uniform distribution (which they
# should if the null hypothesis is true for all 27 candidates) by applying the
# inverse cumulative distribution function for a normal distribution to the
# p-values and then using a Q-Q plot to check for normality
qqnorm(qnorm(pvals), pch=19)
qqline(qnorm(pvals))

# conduct a chi-square test on the entire table of votes received over time
chisq.test(votes)

############################################################################

### 4.7: Moving beyond hypothesis testing

# nothing to replicate here

############################################################################
