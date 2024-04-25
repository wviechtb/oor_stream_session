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

# typically, there are (at least) two libraries:

# 1) the 'system library' into which the packages are installed that come with
#    R by default (e.g., the 'base', 'stats', 'utils' packages)
# 2) the 'user library' into which additional packages are installed

# to get more information about installed packages
installed.packages()

# only show where installed, version, and the 'priority'
installed.packages()[,c("LibPath", "Version", "Priority")]

# the 'base' and 'recommended' packages (see 'Priority' column) are installed
# with R automatically; it is also possible to install an updated version of
# recommended packages (if there is an update)

# list loaded packages (have to explicitly use print())
print(.packages())

# can also use search(), but strictly speaking, this is showing the 'search
# path', that is, the locations where R is looking for an object
search()

# for example, when we type
mtcars

# then R is going through the search path to find an object with that name
# (which happens to be part of the 'datasets' package)

# load the 'boot' package
library(boot)

# check which packages are loaded now
print(.packages())

# install the 'lme4' package (https://cran.r-project.org/package=lme4)
install.packages("lme4")

# if possible, R will install a package from the 'binary version', which
# avoids having to compile C/C++/Fortan code (if the package makes use of such
# code); this will typically be the case under Windows and macOS;
# alternatively, if one has the appropriate tools installed, one can also
# install packages from the 'source version'




# name clashes (masking)

############################################################################



## 13.1: Standard packages

