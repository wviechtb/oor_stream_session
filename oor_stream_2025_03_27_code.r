############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-03-27
#
# Topic(s):
# - new features in the metafor package (version 4.8-0)
#
# last updated: 2025-03-27

############################################################################

# install the metafor package
#install.packages("metafor")

# load the metafor package
library(metafor)

# copy BCG vaccine meta-analysis data to 'dat'
dat <- dat.bcg

# tpos  - number of TB positive cases in the treated (vaccinated) group
# tneg  - number of TB negative cases in the treated (vaccinated) group
# cpos  - number of TB positive cases in the control (non-vaccinated) group
# cneg  - number of TB negative cases in the control (non-vaccinated) group
#
# these variables denote the values in 2x2 tables of the form:
#
#           TB+    TB-
#         +------+------+
# treated | tpos | tneg |
#         +------+------+
# control | cpos | cneg |
#         +------+------+

# calculate log risk ratios and corresponding sampling variances (and use
# the 'slab' argument to store study labels as part of the data frame)
dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg,
              data=dat, slab=paste(author, year, sep=", "))

### fit random-effects model
res <- rma(yi, vi, data=dat)