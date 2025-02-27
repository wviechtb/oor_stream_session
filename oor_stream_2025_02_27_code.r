############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-02-27
#
# Topic(s):
# - Statistical Rethinking (https://xcelab.net/rm/)
# - Section(s): 5.1 - ?
#
# last updated: 2025-02-27

############################################################################

### 5.1: Spurious association

# load the rethinking package
library(rethinking)

# get the WaffleDivorce dataset and put it into 'dat'
dat <- get(data(WaffleDivorce))

# standardize some variables
dat$D <- c(scale(dat$Divorce))
dat$M <- c(scale(dat$Marriage))
dat$A <- c(scale(dat$MedianAgeMarriage))