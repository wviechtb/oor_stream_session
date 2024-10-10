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
