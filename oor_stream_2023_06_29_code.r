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

### 7.3: Accessing builtin datasets

# see what built-in datasets are currently available
data()

# one of the datasets is called 'mtcars'; we can directly access the dataset
mtcars

# for some packages with built-in datasets, one has to use the data() function
# to actually load the dataset; this isn't really necessary, but just for
# illustration purposes
data(mtcars)

############################################################################

### 7.4: Editing data

# to inspect a dataset, we can use the View() command
View(mtcars)

# edit the 'dat' datset and assign the changed dataset to 'dat2'
dat2 <- edit(dat)

# never use this! this creates a non-reproducible workflow; if you need to
# make adjustments to the dataset, you should do this with code (which you can
# then always re-run to make the same changes); for illustration purposes, say
# that the 3rd house actually had 7 rooms (and not 5), then we can fix this
dat2 <- dat
dat2$Rooms[3] <- 7
dat2

# also, say that the Age value for the 4th house is unknown
dat2$Age[4] <- NA

# add a new variable to dat2 with some comments about the houses
dat2$Comment <- ""
dat2$Comment[1] <- "beautiful garden"
dat2$Comment[3] <- "no basement"
dat2$Comment[4] <- "blah # blup"

############################################################################

### Saving data

# save dat2 as a tab-delimited plain text file without row names and using
# 'blank' for missing values (not "NA")
write.table(dat2, file="houses_edit.txt", row.names=FALSE, na="", sep="\t")

# now we can read the data back into R with
dat3 <- read.table("houses_edit.txt", header=TRUE, sep="\t")
dat3

# note: when exporting dat2 to houses_edit.txt, strings are in quotes;
# however, other software from which we might export a dataset may not do
# this; then the # symbol in the dataset will cause a problem, because
# read.table() by default treats everything after # as a comment and ignores
# everything after it; you can switch off this behavior in read.table() with
# comment.char=""

############################################################################

# the # symbol in external datasets

# SPSS, Excel, and other file formats
# reading in Unicode symbols
