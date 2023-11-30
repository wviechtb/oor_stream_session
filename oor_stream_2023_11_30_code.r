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

# look at the documentation of the forest() function (when passing a model
# object to forest(), the forest.rma() function is used)
help(forest.rma)

# create a very simple forest plot based on the results from the model
forest(res)

# add a study header and a header for the annotations
forest(res, header=TRUE)

# suppress the annotations on the right-hand side
forest(res, header=TRUE, annotate=FALSE)

# suppress the summary estimate on the bottom
forest(res, header=TRUE, addfit=FALSE)

# show the prediction interval around the summary polygon as a dotted line
forest(res, header=TRUE, addpred=TRUE)

# show the weights
forest(res, header=TRUE, showweights=TRUE)

# the header argument can also be a string or a two-element character vector
forest(res, header="Author, Year")
forest(res, header=c("Author, Year", "Log Risk Ratio [95% CI]"))

# suppress the reference line at 0
forest(res, header=TRUE, refline=NA)

# put the reference line at the pooled estimate
forest(res, header=TRUE, refline=coef(res))

# refline can even be a vector to add multiple reference lines
forest(res, header=TRUE, refline=c(0, coef(res)))

# adjust the x-axis label
forest(res, header=TRUE, xlab="Log Relative Risk")

# can also label the endpoints of the x-axis limits
forest(res, header=TRUE, xlab=c("(favors treatment)", "Log Risk Ratio", "(favors control)"))

############################################################################


forest(res, addpred=TRUE, xlim=c(-16,7), at=seq(-3,2,by=1), shade="zebra",
       ilab=cbind(tpos, tneg, cpos, cneg), ilab.xpos=c(-9.5,-8,-6,-4.5),
       cex=0.75, header="Author(s) and Year")
text(c(-9.5,-8,-6,-4.5), res$k+2, c("TB+", "TB-", "TB+", "TB-"), cex=0.75, font=2)
text(c(-8.75,-5.25),     res$k+3, c("Vaccinated", "Control"),    cex=0.75, font=2)

############################################################################

# discussion points:
# - talk about the alignment of the annotations
# - explain difference between the different forest functions in metafor
# - forest plots for models with moderators
# - use of forest plots outside of meta-analysis

############################################################################
