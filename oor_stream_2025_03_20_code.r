############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-03-20
#
# Topic(s):
# - the tinyplot package
# - the tidyplots package (if there is time)
#
# last updated: 2025-03-20

############################################################################

### tinyplot package

# relevant links:
# - https://cran.r-project.org/package=tinyplot
# - https://grantmcdermott.com/tinyplot/

# install the tinyplot package
#install.packages("tinyplot")

# actually, we will install the version that is available on the R-universe
# website, since it is slightly newer
install.packages("tinyplot", repos="https://grantmcdermott.r-universe.dev")

# install the palmerpenguins packages
install.packages("palmerpenguins")

# load the packages
library(tinyplot)
library(palmerpenguins)

# copy the penguins dataset to dat (and make it a regular data frame)
dat <- data.frame(penguins)

# frequency table of the species variable
table(dat$species)

# a default scatterplot of two variables against each other
plot(bill_length_mm ~ flipper_length_mm, data=dat)

# a bit of customization
plot(bill_length_mm ~ flipper_length_mm, data=dat, pch=21, bg="gray",
     xlab="Flipper length (mm)", ylab="Bill length (mm)", bty="l", las=1)

# say we want to recreate the figure shown here: https://allisonhorst.github.io/palmerpenguins/

plot(bill_length_mm ~ flipper_length_mm, data=dat, type="n",
     xlab="Flipper length (mm)", ylab="Bill length (mm)", bty="l", las=1)

grid()

# so Adelie -> darkorange, Chinstrap -> purple, Gentoo -> cyan4

# same idea also works for using different plotting symbols for the groups

cols <- apply(rbind(col2rgb(c("darkorange","purple","cyan4")), 200), 2,
              function(x) rgb(x[1], x[2], x[3], x[4], maxColorValue=255))

points(bill_length_mm ~ flipper_length_mm, data=penguins,
       pch=c(19,17,15)[species], col=cols[species])

# note: we do not have to use penguins$species when using the 'data' argument

legend("bottomright", pch=c(19,17,15), col=c("darkorange","purple","cyan4"),
       legend=c("Adelie","Chinstrap","Gentoo"), bty="n", title="Penguin species")




# examples based on the Quickstart (https://grantmcdermott.com/tinyplot/)




############################################################################

