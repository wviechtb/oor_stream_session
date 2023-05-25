############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-05-25
#
# Topic(s):
# - An Introduction to R
#   https://cran.r-project.org/doc/manuals/r-release/R-intro.html
# - Section(s): 5.1 - ?
#
# last updated: 2023-05-25

############################################################################

### 5.1: Arrays

# create a vector with 1500 random draws from a standard normal distribution
z <- rnorm(1500)

# look at the first 15 values of z
head(z, 15)

# attach three dimensions to this vector with the first dimension having
# length 3, the second length 5, and the third length 100
dim(z) <- c(3,5,100)
z

# check what the attributes of z are
attributes(z)

# we see that z now as a dimension attribute, which is the 'dimension vector'

# another example with a vector that just has the numbers 1 through 24
a <- 1:24
a

# again attach dimensions to this vector
dim(a) <- c(3,4,2)
a

# create a one-dimensional array
a <- 1:24
a
attributes(a)
dim(a) <- 24
a
attributes(a)
class(a)

############################################################################

### 5.2: Array indexing. Subsections of an array

# back to the 3*4*2 array, but let's use some more interesting numbers
a <- round(rnorm(24), digits=3)
dim(a) <- c(3,4,2)
a

# select the second level of the first dimension
a[2,,]

# select the second level of the second dimension
a[,2,]

# get the dimension vector
dim(a)

# internally, 'a' is still stored as a vector; we just attached dimensions to
# it which prints the vector in array format, but we can still refer to
# individual elements of the vector using [] notation
a[c(2,3,8,13)]

############################################################################

### 5.3: Index matrices

#
x <- array(round(rnorm(20), digits=3), dim=c(4,5))