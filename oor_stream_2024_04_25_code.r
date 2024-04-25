############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-04-25
#
# Topic(s):
# - An Introduction to R
#   https://cran.r-project.org/doc/manuals/r-release/R-intro.html
# - Section(s): 13.1 - ?
#
# last updated: 2024-04-25

############################################################################

### 13: Packages

# see which packages are installed
library()

# terminology:
# - package = book
# - library = place where you store books
# (don't say you are using the 'xyz library' for your analysis!)


# another way

installed.packages()

# only show where installed, version, and the 'priority'

installed.packages()[,c("LibPath", "Version", "Priority")]

# the 'base' and 'recommended' packages (see 'Priority' column) are installed
# with R automatically; it is also possible to install an updated version of
# recommended packages (if there is an update)

# in RStudio, there is also the 'Packages' pane (bottom-right)

# list loaded packages (have to explicitly use print())

print(.packages())



# name clashes (masking)

############################################################################



## 13.1: Standard packages

