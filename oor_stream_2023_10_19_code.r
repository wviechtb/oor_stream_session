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

### look at some other optins can that be set/adjusted

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

# specify how various elements of the output should be rounded
setmfopt(digits = c(est=2, se=3, test=2, pval=3, ci=2, var=3, sevar=3, fit=3, het=3))

# re-examine the output
res
predict(res, transf=exp)

# see help(misc_options) for further details

# for example, when loading the crayon package, we can get styled output
library(crayon)

# re-examine the output
res
predict(res, transf=exp)

# create forest plot
forest(res, atransf=exp, at=log(c(0.05, 0.25, 1, 4)), xlim=c(-16,6),
       ilab=cbind(tpos, tneg, cpos, cneg), ilab.xpos=c(-9.5,-8,-6,-4.5),
       cex=0.9, header="Author(s) and Year", shade=TRUE)
op <- par(cex=0.9, font=2)
text(c(-9.5,-8,-6,-4.5), res$k+2, c("TB+", "TB-", "TB+", "TB-"))
text(c(-8.75,-5.25),     res$k+3, c("Vaccinated", "Control"))
par(op)

# create funnel plot
funnel(res, ylim=c(0,0.8), las=1, digits=list(3L,1),
       atransf=exp, at=log(c(0.125, 0.25, 0.5, 1, 2, 4)))

# we can force plots to have a dark background
setmfopt(theme="dark")

# create forest plot
forest(res, atransf=exp, at=log(c(0.05, 0.25, 1, 4)), xlim=c(-16,6),
       ilab=cbind(tpos, tneg, cpos, cneg), ilab.xpos=c(-9.5,-8,-6,-4.5),
       cex=0.9, header="Author(s) and Year", shade=TRUE)
op <- par(cex=0.9, font=2)
text(c(-9.5,-8,-6,-4.5), res$k+2, c("TB+", "TB-", "TB+", "TB-"))
text(c(-8.75,-5.25),     res$k+3, c("Vaccinated", "Control"))
par(op)

# create funnel plot
funnel(res, ylim=c(0,0.8), las=1, digits=list(3L,1),
       atransf=exp, at=log(c(0.125, 0.25, 0.5, 1, 2, 4)))

# can also have custom themes
setmfopt(theme="custom", fg="yellow", bg="darkblue")

# all colors used in plots created by metafor are chosen relative to the
# foreground and background colors

# create funnel plot
funnel(res, ylim=c(0,0.8), las=1, digits=list(3L,1),
       atransf=exp, at=log(c(0.125, 0.25, 0.5, 1, 2, 4)))

# in RStudio, can set theme to "auto" in which case the foreground and
# background colors of plots are chosen according to the RStudio theme
setmfopt(theme="auto")

# create funnel plot
funnel(res, ylim=c(0,0.8), las=1, digits=list(3L,1),
       atransf=exp, at=log(c(0.125, 0.25, 0.5, 1, 2, 4)))

############################################################################

### updates to the fsn() function

# copy the dat.hackshaw1998 dataset to dat
dat <- dat.hackshaw1998

# fit equal-effects model
res <- rma(yi, vi, data=dat, method="EE")
res
predict(res, transf=exp)

# create funnel plot
funnel(res)

# run the regression test for funnel plot asymmetry
regtest(res)

# file drawer analysis using the Rosenthal approach
fsn(yi, vi, data=dat)

# file drawer analysis using the Orwin approach, calculating the number of
# studies averaging null results that it would take to reduce the pooled odds
# 1.05 (i.e., a 5% increase in the odds of lung cancer in exposed women);
# note: the analysis is done with log odds ratios, so we also have to specify
# the target effect size in log units
fsn(yi, vi, data=dat, type="Orwin", target=log(1.05))

# demonstrate that if we add 104 studies with null results that have
# sampling variances equal to the harmonic mean of the sampling variances of
# the original studies, then we indeed get the target pooled effect size
yi.all <- c(dat$yi, rep(0, 104))
vi.all <- c(dat$vi, rep(1/mean(1/dat$vi), 104))
rma(yi.all, vi.all, method="EE")
