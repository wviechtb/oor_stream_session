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

# everything has turned into a character string, so we have a character
# vector; R uses various rules for doing this sort of 'type coercion'

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

# for a list, length() tells us about the number of elements in the list
length(l)

# an object can have other 'attributes'; for example, lists have an attribute
# called 'names' which gives the names of the list elements
attributes(l)

# numeric vectors can be of different types
x <- c(2.4, 1.8, 5.7, 3.9)
x

# obviously, x is a numeric vector
mode(x)

# but it is a collection of numbers that have a fractional part; in R, this
# means that this numeric vector is of type 'double'
typeof(x)

# we can have another numeric vector that is of type 'integer'; for this, we
# put L after each integer
x <- c(2L, 1L, 5L, 3L)
x
mode(x)
typeof(x)

# if we don't use L, then the numeric vector is of type 'double'
x <- c(2, 1, 5, 3)
x
mode(x)
typeof(x)

# cannot mix doubles and integers in the same vector
x <- c(2, 1, 5L, 3)
x
mode(x)
typeof(x)

# if we try, then the integers get 'promoted' to doubles

# create a vector of the numbers 0, 1, ..., 9
z <- 0:9
z

# when we use : to create a numeric sequence, it creates an integer vector
typeof(z)

# turn the numeric (integer) vector z into a character vector
digits <- as.character(z)
digits

# this is doing 'explicit type coercion' (note that we already saw 'implicit
# type coercion' earlier, for example when mixing numbers and strings in a
# vector or when mixing doubles and integers in a vector)

# you can even turn a character vector into a numeric vector (either a double
# or integer vector) using as.double() or as.integer()
d <- as.integer(digits)
d

# there is also as.numeric() which always creates a 'double'
typeof(as.numeric(digits))

############################################################################

### 3.2: Changing the length of an object

# we can create an empty numeric vector with numeric()
e <- numeric()
e

# it is of length 0
length(e)

# but this empty vector still has a mode (and a type)
mode(e)
typeof(e)

# similarly, can create empty character or logical vectors
character()
logical()

# note: if you do specify the length of the vector, R initializes these
# different vector types to different default values
numeric(10)
character(10)
logical(10)

# assign the value 17 to the third element of e; since right now, e is of
# length 0, R will happily change the length of the vector to 3, but the first
# and second element will be missing (NA)
e[3] <- 17
e
