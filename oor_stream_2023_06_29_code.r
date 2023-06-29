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

# note: the example dataset shown at the beginning of this section has 'row
# names' but this is quite specific to R and not what you usually would have
# in such a dataset that was exported for example from Excel, SPSS, or some
# other software; therefore, the houses.txt file that we will work with does
# not contain these row names

# read in the data from the houses.txt file
dat <- read.table("houses.txt")
