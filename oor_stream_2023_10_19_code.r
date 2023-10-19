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

# the approach above for setting such options is outdated

# get the metafor package options
getmfopt()

# set the space option to FALSE
setmfopt(space=FALSE)

# re-examine the output
res

# set the space option back to TRUE
setmfopt(space=TRUE)

# re-examine the output
res

############################################################################

# look at the documentation for setmfopt() to see what other options can be set
help(setmfopt)

# re-examine the output
res

# set the number of digits to print to 2 in the output
print(res, digits=2)

# we can set the number of digits to show globally
setmfopt(digits=2)

# re-examine the output
res
predict(res, transf=exp)
