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

# check which packages are loaded now
print(.packages())

# install the 'lme4' package (https://cran.r-project.org/package=lme4)
install.packages("lme4")

# if possible, R will install a package from the 'binary version', which
# avoids having to compile C/C++/Fortan code (if the package makes use of such
# code); this will typically be the case under Windows and macOS;
# alternatively, if one has the appropriate tools installed, one can also
# install packages from the 'source version'

# load the 'lme4' package
library(lme4)

# to get a list of help topics of an installed package, one can do as
# described in the manual using help.start(), but more directly, we can do the
# following
help(package="lme4")

############################################################################

## 13.1: Standard packages

# as discussed above, these are the packages that come with R

############################################################################

## 13.2: Contributed packages and CRAN

# the 'recommended' packages also come with R; everything else are additional
# packages installed by the user

############################################################################

## 13.3: Namespaces

# install the 'psych' and 'lavaan' packages
install.packages("psych")
install.packages("lavaan")

# a note about 'masking' (or name clashes): different packages may contain
# functions that have the same name; for example:

library(psych)
library(lavaan)

# - note that it says that function 'cor2cov' has been masked
# - what has happened is that both packages have a function called 'cor2cov'
# - so when you now use the cor2cov function, the one from the lavaan package
#   will be used (i.e., always the one from the package loaded last)
# - but what if you want to use the 'cor2cov' function from the psych package?
# - then you can use psych::cor2cov() to explicitly tell R to use the cor2cov
#   function from the psych package
# - the more packages you load, the more likely it is that two packages will
#   contain functions with the same name and hence that masking will occur
# - to avoid the headaches that this can create, only load packages at the
#   beginning of your script that you really need

############################################################################


