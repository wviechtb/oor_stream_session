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

# set up the plot (but don't actually show the points)
plot(bill_length_mm ~ flipper_length_mm, data=dat, type="n",
     xlab="Flipper length (mm)", ylab="Bill length (mm)", bty="l", las=1)

# add a grid
grid()

# specify the three colors with some transparency added
species <- c("Adelie", "Chinstrap", "Gentoo")
cols <- c("darkorange","purple","cyan4")
cols.t <- apply(rbind(col2rgb(cols), 200), 2,
                function(x) rgb(x[1], x[2], x[3], x[4], maxColorValue=255))

# now add the points, with different plotting symbols and colors for the three
# species (Adelie = darkorange, Chinstrap = purple, Gentoo = cyan4)
points(bill_length_mm ~ flipper_length_mm, data=penguins,
       pch=c(19,17,15)[species], col=cols[species])

# fit the model that allows for different intercepts and slopes for the species
res <- lm(bill_length_mm ~ 0 + species + flipper_length_mm:species, data=dat)

for (i in 1:length(species)) {
   xs <- range(dat$flipper_length_mm[dat$species == species[i]], na.rm=TRUE)
   pred <- predict(res, newdata=data.frame(species=species[i], flipper_length_mm=xs))
   lines(xs, pred, lwd=6, col=cols[i])
}

# add a legend
legend("bottomright", pch=c(19,17,15), col=cols, legend=species,
       bty="n", title="Penguin species")

mtext("Flipper and bill length", side=3, adj=0, line=2.5)
mtext("Dimensions for Adelie, Chinstrap, and Gentoo Penguins at Palmer Station LTER", side=3, adj=0, line=1.5, cex=0.8)



# examples based on the Quickstart (https://grantmcdermott.com/tinyplot/)




############################################################################

