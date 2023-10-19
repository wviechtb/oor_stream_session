############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-10-19
#
# Topic(s):
# - Recent Updates to the metafor Package
# - https://www.metafor-project.org
#
# last updated: 2023-10-19

############################################################################

# install the metafor package
#install.packages("metafor")

# load the metafor package
library(metafor)

############################################################################

### illustrate the setmfopt() and getmfopt() functions

# calculate log risk ratios and corresponding sampling variances
dat <- escalc(measure="RR", ai=tpos, bi=tneg,
                            ci=cpos, di=cneg, data=dat.bcg,
                            slab=paste0(author, ", ", year))
dat

# random-effects model
res <- rma(yi, vi, data=dat)
res

# average risk ratio with 95% CI
predict(res, transf=exp)

# create .rmspace object and set it to TRUE
.rmspace <- TRUE

# re-examine the output
res

# note that the leading and trailing empty lines are gone now (this can be
# useful when creating for example Rmarkdown documents with output from
# metafor, where these empty lines are superfluous)

# remove the .rmspace object
rm(.rmspace)

# re-examine the output
res


############################################################################
