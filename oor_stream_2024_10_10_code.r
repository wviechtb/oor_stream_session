############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-10-10
#
# Topic(s):
# - Statistical Rethinking (https://xcelab.net/rm/)
# - Section(s): 4.1 - ?
#
# last updated: 2024-10-10

############################################################################

### 4.1: Why normal distributions are normal

# simulate 16 flips of a fair coin 1000 times
tails <- rbinom(1000, size=16, prob=0.5)

# translate the number of tails observed into the position relative to the center line
pos <- tails * 1 + (16 - tails) * -1

# frequency table of the positions observed
table(pos)

# create a barplot of the frequencies
barplot(table(factor(pos, levels=seq(-16,16,by=2))))

# proportion of people standing on the center line (pos equal to 0)
mean(pos == 0)

# proportion of people standing 5 yards to the left/right of the center line
mean(pos == 5)
mean(pos == -5)

# note: for an even number of flips, pos must be a multiple of 2, so it is not
# possible to stand 5 yards away from the center line

## 4.1.1: Normal by addition

# simulate the sum of 16 random values from a uniform(0,1) distribution 1000 times
pos <- replicate(1000, sum(runif(16,-1,1)))

# histogram of the positions
hist(pos, main="", breaks=40)

# plot of a kernel density estimate of the distribution
plot(density(pos), lwd=4, main="")

# superimpose a normal distribution with the observed mean and sd of the pos values
curve(dnorm(x, mean=mean(pos), sd=sd(pos)), lwd=2, add=TRUE, col="red")

# repeat the simulation but now save the size of each step (instead of their total sum)
pos <- replicate(1000, runif(16,-1,1))

# examine the results for the first 5 people
pos[,1:5]

# take the cumulative sum across columns
pos <- apply(pos, 2, cumsum)

# examine the results for the first 5 people
pos[,1:5]

# Figure 4.2: plot of the position of the people over time
plot(NA, xlim=c(0,16), ylim=c(-1,1)*max(abs(pos)), xlab="step number", ylab="position")
apply(pos, 2, function(y) lines(0:16, c(0,y), col=rgb(30,89,174,50,maxColorValue=255)))
abline(v=c(4,8,16), lty="dashed", lwd=2)

# histogram of the positions after 2, 4, 8, and 16 steps
par(mfrow=c(2,2))
plot(density(pos[2,], bw=0.2), xlim=c(-6,6), lwd=4, col="#1e59ae", xlab="position", ylab="density", main="2 Step")
curve(dnorm(x, mean=mean(pos[2,]), sd=sd(pos[2,])), lwd=2, add=TRUE, col="gray")
plot(density(pos[4,], bw=0.2), xlim=c(-6,6), lwd=4, col="#1e59ae", xlab="position", ylab="density", main="4 Steps")
curve(dnorm(x, mean=mean(pos[4,]), sd=sd(pos[4,])), lwd=2, add=TRUE, col="gray")
plot(density(pos[8,], bw=0.2), xlim=c(-6,6), lwd=4, col="#1e59ae", xlab="position", ylab="density", main="8 Steps")
curve(dnorm(x, mean=mean(pos[8,]), sd=sd(pos[8,])), lwd=2, add=TRUE, col="gray")
plot(density(pos[16,], bw=0.2), xlim=c(-6,6), lwd=4, col="#1e59ae", xlab="position", ylab="density", main="16 Steps")
curve(dnorm(x, mean=mean(pos[16,]), sd=sd(pos[16,])), lwd=2, add=TRUE, col="gray")
par(mfrow=c(2,2))
