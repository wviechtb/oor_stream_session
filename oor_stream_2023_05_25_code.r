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

# create a two-dimension array (i.e., a matrix)
x <- array(round(rnorm(20), digits=3), dim=c(4,5))
x

# create an index matrix
i <- array(c(1:3,3:1), dim=c(3,2))
i

# using the index array, we extract the value in the first row and third
# column, the value in the second row and second column, and the value in the
# third row and first column
x[i]

# replace those values with 0
x[i] <- 0
x

# see what happens when elements in the index vector are 0 or NA
i[1,1] <- 0
i[3,2] <- NA
i
x[i]

# create a logical matrix indicating which elements are greater 0
l <- x > 0
l

# use this logical matrix for picking out elements from x
x[l]

############################################################################

### 5.4: The array() function

# create an array using array()
h <- round(rnorm(24), digits=3)
h
Z <- array(h, dim=c(3,4,2))
Z

# illustrate recycling
h <- round(rnorm(7), digits=3)
h
Z <- array(h, dim=c(3,4,2))
Z

# but this does not work
dim(h) <- c(3,4,2)

# recycling also happens when the input vector is a single value
Z <- array(0, c(3,4,2))
Z

# mathematical operations are done element-by-element
A <- array(sample(1:4,6,replace=TRUE), dim=c(3,2))
B <- array(sample(1:4,6,replace=TRUE), dim=c(3,2))
C <- array(sample(1:4,6,replace=TRUE), dim=c(3,2))
A
B
C
D <- 2*A*B + C + 1
D

############################################################################

### 5.5: The outer product of two arrays

# an example of creating an outer product
a <- array(c(2,4,3,1), dim=c(2,2))
b <- array(c(12,10,9,11,7,10), dim=c(3,2))
a
b

a %o% b
outer(a, b, "*")

# let's consider a simpler example where each input to outer() is just a vector
a <- c(2,4,3,1)
b <- c(12,10,9,11,7,10)
a
b
outer(a, b, "*")

# a fancier example
x <- seq(-pi, pi, len=50)
y <- x
fun <- function(x, y) cos(y)/(1 + x^2)
z <- outer(x, y, fun)
dim(z)
persp(x, y, z, theta=45, phi=35, col="lightgray")

# an example: determinants of 2 by 2 single-digit matrices
ad <- outer(0:9, 0:9)
bc <- outer(0:9, 0:9)
ad
bc
detmat <- outer(d, d, "-")
freqtab <- table(detmat)
freqtab
plot(freqtab, xlab="Determinant", ylab="Frequency")
hist(detmat, breaks=seq(-81,81,length=30))

############################################################################

### 5.6: Generalized transpose of an array

# illustrate transposing with the simple case of a matrix
A <- array(1:6, dim=c(3,2))
A
aperm(A, c(2,1))
t(A)

############################################################################

### 5.7: Matrix facilities

# create a matrix
A <- matrix(1:6, nrow=3, ncol=2)

# number of rows and columns
nrow(A)
ncol(A)

# or just use dim()
dim(A)

## 5.7.1: Matrix multiplication

# create another matrix B which has as many rows as there are columns in A
B <- matrix(c(7,3,4,6,2,3,8,1), nrow=2, ncol=4)

# multiply the two matrices
A
B
A %*% B

A %*% c(6,2,4)

