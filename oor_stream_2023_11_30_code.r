############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-11-30
#
# Topic(s):
# - Drawing forest plots with the metafor package
# - https://www.metafor-project.org
#
# last updated: 2023-11-30

############################################################################

# install the metafor package
#install.packages("metafor")

# load the metafor package
library(metafor)

############################################################################

# copy the BCG dataset to 'dat' and examine the data
dat <- dat.bcg
dat

# calculate log risk ratios and corresponding sampling variances
dat <- escalc(measure="RR", ai=tpos, bi=tneg,
                            ci=cpos, di=cneg, data=dat,
                            slab=paste0(author, ", ", year))
dat

# random-effects model
res <- rma(yi, vi, data=dat)
res

# average risk ratio with 95% confidence and prediction interval
predict(res, transf=exp, digits=2)

# create a very simple forest plot based on the results from the model
forest(res, header=TRUE)

# suppress the annotations on the right-hand side
forest(res, header=TRUE, annotate=FALSE)


forest(res, header=TRUE, addpred=TRUE)

forest(res, addpred=TRUE, xlim=c(-16,7), at=seq(-3,2,by=1), shade="zebra",
       ilab=cbind(tpos, tneg, cpos, cneg), ilab.xpos=c(-9.5,-8,-6,-4.5),
       cex=0.75, header="Author(s) and Year")
text(c(-9.5,-8,-6,-4.5), res$k+2, c("TB+", "TB-", "TB+", "TB-"), cex=0.75, font=2)
text(c(-8.75,-5.25),     res$k+3, c("Vaccinated", "Control"),    cex=0.75, font=2)

############################################################################

# discussion points:
# - explain difference between the different forest functions in metafor
# - forest plots for models with moderators
# - use of forest plots outside of meta-analysis

############################################################################
