############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-05-11
#
# Topic(s):
# - An Introduction to R
#   https://cran.r-project.org/doc/manuals/r-release/R-intro.html
# - Section(s): 3.1 - ?
#
# last updated: 2023-05-11

############################################################################

### 3.1: Intrinsic attributes: mode and length

# create a numeric vector and check its 'mode'
x <- c(1, 3, 2, 4, 5, 3)
mode(x)

# check if 'x' is a vector
is.vector(x)

# we can also check if something is an 'atomic vector'
is.atomic(x)

# try to mix numerical values with a string in the same vector
x <- c(1, 3, 2, "Bob", 4, 5)
x
mode(x)
is.vector(x)
is.atomic(x)

# everything has turned into a character string, so we have a character vector

# a numeric vector with a missing value
x <- c(1, 3, 2, NA, 5, 3)
mode(x)

# the 4th value is missing and it is a numeric missing value
x[4]

# in R, this can be explicitly stated as
NA_real_

# a character vector with a missing value
x <- c("Bob", "Sue", NA, "Gil")
mode(x)

# the 3rd value is missing and it is a character missing value
x[3]

# in R, this can be explicitly stated as
NA_character_

# list all elements in the workspace
ls()

# remove x
rm(x)

# list all elements in the workspace
ls()

# character(0) indicates a character vector with no elements, so this shows
# that there are no elements in the workspace

# create a list with three elements
l <- list(x = c(1,3,2,5), y = c("Bob","Sue"), z = 42)
l

# the mode of a list is 'list' (surprise!)
mode(l)

# a list is defined to be a vector in R, but it is not an atomic vector
is.vector(l)
is.atomic(l)

# create the numeric vector 'x' and take the mean of its values
x <- c(1, 3, 2, 4, 5, 3, 3, 4, 1, 2, 3, 1, 5, 4, 5, 8, 4, 5)
fivenum(x)

# fivenum() is a function or a 'command' in R; functions in R are also
# objects, which you can inspect, manipulate, and create new ones
fivenum

# can also check the mode of such an object
mode(fivenum)

# but (obviously) they are not (atomic) vectors
is.vector(fivenum)
is.atomic(fivenum)

# use length() to find the number of elements in a vector
length(x)