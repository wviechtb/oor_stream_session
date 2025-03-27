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

# inspect the dataset
dat

# tpos - number of TB positive cases in the treated (vaccinated) group
# tneg - number of TB negative cases in the treated (vaccinated) group
# cpos - number of TB positive cases in the control (non-vaccinated) group
# cneg - number of TB negative cases in the control (non-vaccinated) group
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
dat

# fit random-effects model
res <- rma(yi, vi, data=dat)
res

# obtain the 95% prediction interval
predict(res)

# back-transform the results to the risk ratio scale
predict(res, transf=exp, digits=2)

# forest plot with extra annotations
forest(res, atransf=exp, at=log(c(0.05, 0.25, 1, 4)), xlim=c(-16,6),
       ilab=cbind(tpos, tneg, cpos, cneg), ilab.lab=c("TB+","TB-","TB+","TB-"),
       ilab.xpos=c(-9.5,-8,-6,-4.5), cex=0.9, header="Author(s) and Year",
       mlab="", shade=TRUE, addpred=TRUE)
text(c(-8.75,-5.25), res$k+2.8, c("Vaccinated", "Control"), cex=0.9, font=2)

# add text with Q-value, dfs, p-value, I^2, and tau^2 estimate
text(-16, -1, pos=4, cex=0.9, bquote(paste(
      "RE Model (Q = ", .(fmtx(res$QE, digits=2)),
      ", df = ", .(res$k - res$p), ", ",
      .(fmtp2(res$QEp)), "; ",
      I^2, " = ", .(fmtx(res$I2, digits=1)), "%, ",
      tau^2, " = ", .(fmtx(res$tau2, digits=2)), ")")))

# the dotted horizontal line around the summary polygon/diamond corresponds to
# the prediction interval; however, this looks similar to the lines around the
# individual study estimates above, which are confidence intervals, which has
# the potential to lead to some confusion / misinterpretation

# therefore, some additional ways of visualizing the prediction interval have
# been added to the metafor package, via the 'predstyle' argument

# show the prediction interval as a bar below the summary polygon
forest(res, atransf=exp, at=log(c(0.05, 0.25, 1, 4)), xlim=c(-16,6),
       ilab=cbind(tpos, tneg, cpos, cneg), ilab.lab=c("TB+","TB-","TB+","TB-"),
       ilab.xpos=c(-9.5,-8,-6,-4.5), cex=0.9, header="Author(s) and Year",
       shade=TRUE, addpred=TRUE, predstyle="bar")

# show the prediction interval as a shaded bar below the summary polygon,
# where the shading corresponds to the density of the prediction distribution
forest(res, atransf=exp, at=log(c(0.05, 0.25, 1, 4)), xlim=c(-16,6),
       ilab=cbind(tpos, tneg, cpos, cneg), ilab.lab=c("TB+","TB-","TB+","TB-"),
       ilab.xpos=c(-9.5,-8,-6,-4.5), cex=0.9, header="Author(s) and Year",
       shade=TRUE, addpred=TRUE, predstyle="shade")

# show the entire predictive distribution below the summary polygon
forest(res, atransf=exp, at=log(c(0.05, 0.25, 1, 4)), xlim=c(-16,6),
       ilab=cbind(tpos, tneg, cpos, cneg), ilab.lab=c("TB+","TB-","TB+","TB-"),
       ilab.xpos=c(-9.5,-8,-6,-4.5), cex=0.9, header="Author(s) and Year",
       shade=TRUE, addpred=TRUE, predstyle="dist")

# compute the estimated probability that the true effect (i.e., the true log
# risk ratio) in a particular study is above 0, that is, opposite in sign to
# where the average true effect is
pred <- predict(res)
pnorm(0, mean=pred$pred, sd=pred$pi.se, lower.tail=FALSE)

############################################################################

# copy the data into 'dat'
dat <- dat.konstantopoulos2011
head(dat, 10)

# fit a multilevel model
res <- rma.mv(yi, vi, random = ~ 1 | district/school, data=dat)
res

# get the prediction interval
predict(res)

# draw a forest plot (note: have to increase the height of the plotting device
# for the plot to look somewhat nice) and add the prediction interval as a bar
forest(res, cex=0.8, efac=c(0,1), predstyle="bar")

# add the entire predictive distribution
forest(res, cex=0.8, efac=c(0,1,1,0.9), xlim=c(-3,2.8), alim=c(-1,1.5), steps=6, predstyle="dist")

############################################################################

# copy the data into 'dat'
dat <- dat.berkey1998
dat

# construct block diagonal var-cov matrix of the observed outcomes based on variables v1i and v2i
V <- vcalc(vi=1, cluster=author, rvars=c(v1i, v2i), data=dat)

# fit multiple outcomes (meta-regression) model
res <- rma.mv(yi, V, mods = ~ 0 + outcome,
              random = ~ outcome | trial, struct="UN", data=dat)

# with models with moderators, the predict function gives the fitted values
# for the individual rows of the dataset
predict(res)

# but we can also specify the moderator values to obtain predicted effects for
# particular combinations of moderator values
predict(res, newmods=newmods=c(1,0))

# but in order to obtain the corresponding prediction intervals, we also have
# to specify the levels of the inner factor of '~ outcome | trial' (i.e., the
# outcome level(s))
pred.AL <- predict(res, newmods=c(1,0), tau2.levels="AL")
pred.PD <- predict(res, newmods=c(0,1), tau2.levels="PD")
pred.AL
pred.PD

# when drawing forest plots based on model objects that contain moderators,
# the fitted values are indicated for each data point as polygons
forest(res, slab=paste(author, year, sep=", "))

# in this case, it would make more sense to add these polygons below the
# estimates for the individual studies
forest(res, slab=paste(author, year, sep=", "), addfit=FALSE, ylim=c(-6,13),
       ilab=outcome, ilab.lab="Outcome")
abline(h=0)
addpoly(pred.AL, rows=-1, mlab="Pooled AL Estimate", predstyle="dist")
addpoly(pred.PD, rows=-4, mlab="Pooled PD Estimate", predstyle="dist")

############################################################################

# let's go back to the BCG vaccine dataset
dat <- dat.bcg

# fit random-effects model (here, we specify in the call to rma() what measure
# to compute and provide the needed inputs)
res <- rma(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat)
res

# draw a L'Abbe plot based on the model
labbe(res)
