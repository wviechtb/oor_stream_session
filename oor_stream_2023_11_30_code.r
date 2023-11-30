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

# can also label the endpoints of the x-axis limits (or also the center)
forest(res, header=TRUE, xlab=c("(favors treatment)", "(favors control)"))
forest(res, header=TRUE, xlab=c("(favors treatment)", "Log Risk Ratio", "(favors control)"))

# one can also specify study labels directly when using the forest() function
forest(res, header=TRUE, slab=paste("Study", 1:13))

# but generally, it is better to specify the study labels when using escalc()
# or when fitting the model (so, in rma(), there is also the possibility to
# specify study labels via the 'slab' argument)

# adjust the label given to the summary estimate
forest(res, header=TRUE, mlab="Summary")

# change the symbol for the observed outcomes to circles
forest(res, header=TRUE, pch=19)

# can specify the background color of 'open' plot symbols with the bg argument
forest(res, header=TRUE, pch=21, bg="gray")

# change the color for the observed outcomes to gray
forest(res, header=TRUE, colout="blue")

# colout can also be a vector, giving a color to each individual study; for
# example, we could use a different color for studies where the estimate is
# significantly different from 0 versus those where it is not; we can get the
# p-values for testing H0: log(RR) = 0 for each study with summary()
summary(dat)

# these we can extract and compare against alpha = 0.05
summary(dat)$pval <= 0.05

# based on this logical vector, we can create a color vector
ifelse(summary(dat)$pval <= 0.05, "red", "black")

# which we then use as the input to the colout argument
forest(res, header=TRUE, colout=ifelse(summary(dat)$pval <= 0.05, "red", "black"))

# or we could color the more recent studies differently
forest(res, header=TRUE, colout=ifelse(year > 1970, "red", "black"))

# or for something very colorful
forest(res, header=TRUE, colout=rainbow(13))

# change the color of the summary polygon and/or its border color
forest(res, header=TRUE, col="gray")
forest(res, header=TRUE, col="gray", border="darkgray")

# change the line type for the confidence intervals to dashed lines
forest(res, header=TRUE, lty="dashed")

# see help(par) for the line type options

# adjust the number of digits to which the annotations and x-axis tick labels
# are rounded (the default is 2L, where L declares 2 to be an integer)
forest(res, header=TRUE, digits=3L)

# when specifying an integer for digits, trailing zeros on the x-axis tick
# labels are dropped; when not explicitly declaring the number to be an
# integer, trailing zeros are not dropped
forest(res, header=TRUE, digits=3)

# can specify a different number of digits for the annotations and x-axis labels
forest(res, header=TRUE, digits=c(3,1))

# change the vertical expansion factor for the CI limits and the summary polygon
forest(res, header=TRUE, efac=1.5)
forest(res, header=TRUE, efac=0.5)
forest(res, header=TRUE, efac=c(0,1))

# make all point sizes equal
forest(res, header=TRUE, psize=1.2)

# there is some more advanced functionality that is available in connection
# with the point sizes, making use of the plim argument; see the documentation
# for further details

# shade the rows of the forest plot zebra-style
forest(res, header=TRUE, shade=TRUE)
forest(res, header=TRUE, shade="zebra")

# shade starting with the second study
forest(res, header=TRUE, shade="zebra2")

# shade all rows
forest(res, header=TRUE, shade="all")

# can adjust the color for the shaded rows
forest(res, header=TRUE, shade=TRUE, colshade="lightblue")

# shade rows where the estimate is significantly different from 0
forest(res, header=TRUE, shade=summary(dat)$pval <= 0.05)

# shade rows 1, 5, and 10
forest(res, header=TRUE, shade=c(1,5,10))

# order the studies by the size of the observed outcomes
forest(res, header=TRUE, order="obs")

# order the studies by their precision
forest(res, header=TRUE, order="prec")

# can also specify a variable for 'order' based on which the studies are sorted
forest(res, header=TRUE, order=year)

# adjust the size of the elements in the plot
forest(res, header=TRUE, cex=1.5)

# for further control over the size of the x-axis title and the x-axis tick
# mark labels, can use the cex.lab and cex.axis arguments

# adjust the x-axis limits to -3 and 3
forest(res, header=TRUE, alim=c(-3,3))

# also adjust the number of x-axis tick marks to 7
forest(res, header=TRUE, alim=c(-3,3), steps=7)

# with the at argument, we can specify the exact position of the tick marks
forest(res, header=TRUE, at=c(-3,-1,0,1,3))

# note: if a CI bound falls outside of the range of the x-axis, this is
# indicated with an arrow symbol
forest(res, header=TRUE, alim=c(-3,1))

# do the back-transformation via an x-axis transformation
forest(res, header=TRUE, atransf=exp)

# so the values from the x-axis (-3, -2, -1, 0, 1, 2) are exponentiated; the
# values given then reflect risk ratios, where for example exp(1) is of the
# same magnitude as exp(-1), but of course in different directions; the x-axis
# is now said to be on a 'log scale'



############################################################################


forest(res, addpred=TRUE, xlim=c(-16,7), at=seq(-3,2,by=1), shade="zebra",
       ilab=cbind(tpos, tneg, cpos, cneg), ilab.xpos=c(-9.5,-8,-6,-4.5),
       cex=0.75, header="Author(s) and Year")
text(c(-9.5,-8,-6,-4.5), res$k+2, c("TB+", "TB-", "TB+", "TB-"), cex=0.75, font=2)
text(c(-8.75,-5.25),     res$k+3, c("Vaccinated", "Control"),    cex=0.75, font=2)

############################################################################

# discussion points:
# - talk about the alignment of the annotations
# - how to show studies with missings
# - explain difference between the different forest functions in metafor
# - forest plots for models with moderators
# - use of forest plots outside of meta-analysis

############################################################################
