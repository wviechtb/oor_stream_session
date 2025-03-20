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

# examples based on the Quickstart (https://grantmcdermott.com/tinyplot/)




############################################################################

