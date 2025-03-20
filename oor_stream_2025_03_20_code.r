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

# specify the three colors and the same colors with some transparency added
species <- c("Adelie", "Chinstrap", "Gentoo")
cols <- c("darkorange","purple","cyan4")
cols.t <- apply(rbind(col2rgb(cols), 200), 2,
                function(x) rgb(x[1], x[2], x[3], x[4], maxColorValue=255))

# now add the points, with different plotting symbols and colors for the three
# species (Adelie = darkorange, Chinstrap = purple, Gentoo = cyan4)
points(bill_length_mm ~ flipper_length_mm, data=penguins,
       pch=c(19,17,15)[species], col=cols.t[species])

# fit the model that allows for different intercepts and slopes for the species
res <- lm(bill_length_mm ~ 0 + species + flipper_length_mm:species, data=dat)

for (i in 1:length(species)) {
   xs <- range(dat$flipper_length_mm[dat$species == species[i]], na.rm=TRUE)
   pred <- predict(res, newdata=data.frame(species=species[i], flipper_length_mm=xs))
   lines(xs, pred, lwd=5, col=cols[i])
}

# add a legend
legend("bottomright", pch=c(19,17,15), col=cols.t, legend=species,
       bty="n", title="Penguin species")

# add text at the top
mtext("Flipper and bill length", side=3, adj=0, line=2.5)
mtext("Dimensions for Adelie, Chinstrap, and Gentoo Penguins at Palmer Station LTER", side=3, adj=0, line=1.5, cex=0.8)

# now let's try to simplify the above a bit by making use of tinyplot functionality

# generally the syntax of tinyplot() (or plt() for short) is like the plot() syntax
plt(bill_length_mm ~ flipper_length_mm, data=dat, pch=21, bg="gray",
    xlab="Flipper length (mm)", ylab="Bill length (mm)")

# however, there are some subtle differences; bty="l" and las=1 do not work
# within the plt() call as above; we can get around this by setting the 'las'
# value first with par(), suppressing the box altogether with frame=FALSE, and
# if we like adding the L box back with box()
par(las=1)
plt(bill_length_mm ~ flipper_length_mm, data=dat, pch=21, bg="gray",
    xlab="Flipper length (mm)", ylab="Bill length (mm)", frame=FALSE)
box(bty="l")

# tinyplot supports specifying a grouping variable as part of the formula
plt(bill_length_mm ~ flipper_length_mm | species, data=dat)

# using themes, we can also change the overall look of the plot, with other
# defaults (e.g., the minimal theme uses horizontal axis labels by default)

# recreate the same figure as above using tinyplot
tinytheme("minimal")
plt(bill_length_mm ~ flipper_length_mm | species, data=dat,
    xlab="Flipper length (mm)", ylab="Bill length (mm)",
    palette=c("darkorange","purple","cyan4"), alpha=0.8, pch=c(19,17,15),
    legend=legend("bottomright", title="Penguin species"),
    main="Flipper and bill length",
    sub="Dimensions for Adelie, Chinstrap, and Gentoo Penguins at Palmer Station LTER")
box(bty="l")
plt_add(type="lm", se=FALSE, lwd=5)

# much shorter / easier code

############################################################################

