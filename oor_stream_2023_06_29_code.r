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

# save 'dat2' to an R data file called houses_edit.rdata
save(dat2, file="houses_edit.rdata")

# remove dat2 from the workspace
rm(dat2)

# load 'dat2' from houses_edit.rdata
load("houses_edit.rdata")
dat2

# disadvantage of .rdata files: they cannot be read into any other software but R
# advantage of .rdata files:
# - they are typically smaller in size than .txt files (due to compression)
# - they can be saved and loaded faster (which is only relevant when the
#   dataset is very large)
# - they contain an exact representation of the object(s) that you saved
#   (e.g., factor and time/date variables are saved as such)

############################################################################

### Reading in data with non-standard symbols

dat4 <- read.table("houses2.txt", header=TRUE, sep="\t")
dat4

############################################################################

### SPSS, Excel, and other file formats

# the 'R Data Import/Export' manual contains a lot of additional information:
# https://cran.r-project.org/doc/manuals/r-release/R-data.html

# for reading in SPSS files, the 'foreign' package has a function called
# read.spss() for reading in SPSS .sav files
library(foreign)
help(read.spss)

# read in the houses_edit.sav file
dat <- read.spss("houses_edit.sav", to.data.frame=TRUE)
dat
str(dat)

# variable 'Cent.heat' which used value labels in SPSS is automatically turned
# into a factor and the variable labels are an attribute of the dataset
dat$Cent.heat
attributes(dat)$variable.labels

# the haven package can also read in SPSS files

# install the haven package if it not already installed
#install.packages("haven")

# load the haven package
library(haven)

# read in the houses_edit.sav file
dat <- read_spss("houses_edit.sav")
dat
str(dat)

# turn the tibble into a regular data frame
dat <- data.frame(zap_formats(zap_label(zap_labels(zap_widths(as_factor(dat))))))
dat
