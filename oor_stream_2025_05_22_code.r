############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-05-22
#
# Topic(s):
# - Statistical Rethinking (https://xcelab.net/rm/)
# - Section(s): 7.1 - ?
#
# last updated: 2025-05-22

############################################################################

# load the rethinking package
library(rethinking)

############################################################################

### 7.1: The problem with parameters

## 7.1.1: More parameters (almost) always improve fit

dat <- data.frame(species = c("afarensis", "africanus", "habilis", "boisei", "rudolfensis", "ergaster", "sapiens"),
                  brain   = c(438, 452, 612, 521, 752, 871, 1350),
                  mass    = c(37.0, 35.5, 34.5, 41.5, 55.5, 61.0, 53.5))
