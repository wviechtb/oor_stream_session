############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-04-10
#
# Topic(s):
# - Statistical Rethinking (https://xcelab.net/rm/)
# - Section(s): 6.1 - ?
#
# last updated: 2025-04-10

############################################################################

# load the rethinking package
library(rethinking)

############################################################################

# Overthinking: Simulated science distortion

set.seed(1914)
N <- 200 # number of grant proposals
p <- 0.1 # proportion to select
nw <- rnorm(N) # simulate newsworthiness scores
tw <- rnorm(N) # simulate trustworthiness scores
s <- nw + tw   # compute the total score s
q <- quantile(s, 1-p) # find the top 10% threshold
selected <- s >= q # select a proposal if it has a top 10% combined score
# plot the scores against each other with blue points for the selected proposals
plot(tw, nw, pch=21, bg=ifelse(selected, "dodgerblue", "white"), bty="l",
     xlab="trustworthiness", ylab="newsworthiness")
cor(tw[selected], nw[selected]) # negative correlation among the selected proposals

############################################################################

### 6.1: Multicollinearity