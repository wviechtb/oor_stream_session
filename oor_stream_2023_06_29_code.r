############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-06-29
#
# Topic(s):
# - An Introduction to R
#   https://cran.r-project.org/doc/manuals/r-release/R-intro.html
# - Section(s): 7.1 - ?
#
# last updated: 2023-06-29

############################################################################

### 7.1: The read.table() function

# check what the current working directory (working folder) is
getwd()

# if this does not correspond to the location of this script and the
# houses.txt datafile, then you need to change the working directory to this
# location with the setwd() command, where you need to replace ... with the
# correct location
setwd("...")

# in RStudio, we can also click on Session - Set Working Directory - To Source
# File Location (this sets the working directory to the location of this
# script)

# note: the example dataset shown at the beginning of this section has 'row
# names' but this is quite specific to R and not what you usually would have
# in such a dataset that was exported for example from Excel, SPSS, or some
# other software; therefore, the houses.txt file that we will work with does
# not contain these row names

# read in the data from the houses.txt file
dat <- read.table("houses.txt", header=TRUE)

# inspect the data that were read in
dat

############################################################################

### 7.2: The scan() function

# read in the data from the houses.txt file using scan(); note: we need to
# skip the first line since it is the header row that contains the variable
# names and not actual data
dat <- scan("houses.txt", list(Price=0,Floor=0,Area=0,Rooms=0,Age=0,Cent.heat=""), skip=1)
dat <- data.frame(dat)
dat

# it is not clear why one would ever want to use this kind of workflow; using
# read.table() is simpler and more convenient

############################################################################

# missing in external datasets
# the # symbol in external datasets

# SPSS, Excel, and other file formats
# reading in Unicode symbols
