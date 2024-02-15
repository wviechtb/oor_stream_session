############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-02-15
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 4.6 - ?
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

# number of voters at each tally (note: each voter could vote for up to 6
# candidates, so the total counts at each tally can be as high as 6 times the
# number of voters)
voters <- c(600, 1200, 2444, 3444, 4444, 5553)

# proportion of votes received at the very end
dat$propend <- dat$tally6 / voters[6]

# sort the dataset by proportion of votes received (in decreasing order, so
# the first row is the person who received the most votes)
dat <- dat[order(dat$propend, decreasing=TRUE),]

# reset the row names to increasing integers
rownames(dat) <- NULL

# Figure 4.5

par(mfrow=c(2,4), mar=c(3,4,2,2))

for (i in 1:8) {

   plot(voters, dat[i,1:6]/voters, type="o", pch=21, bg="gray",
        xlim=c(0,6000), ylim=c(0,0.6), xlab="", ylab="")
   title(dat$candidate[i])

}

# Figure 4.6

par(mfrow=c(2,4), mar=c(3,4,2,2))

for (i in 1:8) {

   plot(voters, dat[i,1:6]/voters, type="o", pch=21, bg="gray",
        xlim=c(0,6000), ylim=c(0,0.6), xlab="", ylab="")
   title(dat$candidate[i])

}
