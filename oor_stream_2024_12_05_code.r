############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-12-05
#
# Topic(s):
# - Statistical Rethinking (https://xcelab.net/rm/)
# - Section(s): 4.4 - ?
#
# last updated: 2024-12-05

############################################################################

### 4.4: Linear prediction

# load the rethinking package
library(rethinking)

# get the Howell1 data and put it into 'dat'
dat <- get(data(Howell1))

# select only those who are 18 years or older
dat <- dat[dat$age >= 18,]

# plot the height of the individuals versus their weight
plot(height ~ weight, data=dat, pch=21, bg="gray")
