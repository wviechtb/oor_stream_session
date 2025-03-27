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

forest(res)

# forest plot with extra annotations
forest(res, atransf=exp, at=log(c(0.05, 0.25, 1, 4)), xlim=c(-16,6),
       ilab=cbind(tpos, tneg, cpos, cneg), ilab.lab=c("TB+","TB-","TB+","TB-"),
       ilab.xpos=c(-9.5,-8,-6,-4.5), cex=0.9, header="Author(s) and Year",
       mlab="", shade=TRUE)
text(c(-8.75,-5.25), res$k+2.8, c("Vaccinated", "Control"), cex=0.9, font=2)

# add text with Q-value, dfs, p-value, I^2, and tau^2 estimate
text(-16, -1, pos=4, cex=0.75, bquote(paste(
      "RE Model (Q = ", .(fmtx(res$QE, digits=2)),
      ", df = ", .(res$k - res$p), ", ",
      .(fmtp2(res$QEp)), "; ",
      I^2, " = ", .(fmtx(res$I2, digits=1)), "%, ",
      tau^2, " = ", .(fmtx(res$tau2, digits=2)), ")")))
