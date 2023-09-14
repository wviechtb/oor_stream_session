############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-09-14
#
# Topic(s):
# - An Introduction to R
#   https://cran.r-project.org/doc/manuals/r-release/R-intro.html
# - Section(s): 8.3 - ?
#
# last updated: 2023-09-14

############################################################################

### 8.3: One- and two-sample tests

# create vectors A and B with the data shown (could also use scan())
A <- c(79.98,80.04,80.02,80.04,80.03,80.03,80.04,79.97,80.05,80.03,80.02,80.00,80.02)
B <- c(80.02,79.94,79.98,79.97,79.97,80.03,79.95,79.97)
A
B

# side-by-side boxplots of the two variables
boxplot(A, B)

# make it look a bit nicer
par(bty="l")
boxplot(list(A=A, B=B), xlab="Method", ylab="Latent Heat (cal/gm)")

# independent samples t-test
t.test(A, B)

# note: this runs Welch's t-test (which does not assume equality of variances
# in the two groups / for the two variables)

# for more details, see:
# https://en.wikipedia.org/wiki/Welch%27s_t-test
# https://en.wikipedia.org/wiki/Student%27s_t-test

# look at the observed variances for the two variables
var(A)
var(B)

# test the equality of the two variances
var.test(A, B)

# run the classical Student's t-test (assuming equal variances)
t.test(A, B, var.equal=TRUE)

# two-sample Wilcoxon test (or Mann-Whitney U test)
# https://en.wikipedia.org/wiki/Mann%E2%80%93Whitney_U_test
wilcox.test(A, B)

# look at the empirical cumulative distribution function of the two variables
plot(ecdf(A), do.points=FALSE, verticals=TRUE, xlim=range(A, B), col="red", lwd=5, main="")
plot(ecdf(B), do.points=FALSE, verticals=TRUE, add=TRUE, col="blue", lwd=5)
text(80.00, 0.77, "Method B", cex=1.5)
text(80.01, 0.25, "Method A", cex=1.5)

# Kolmogorov-Smirnov test (of the maximal vertical distance between the two ecdf)
ks.test(A, B)

# note: these results are slightly different from what it shows in the manual,
# because now the test is based on the exact p-value, not the approximate one;
# to get the same result as in the manual, we can use
ks.test(A, B, exact=FALSE)

# put the data from the example into a data frame
dat <- data.frame(method=c(rep("A",length(A)),rep("B",length(B))), heat=c(A,B))
dat

# rerun everything above but with this data layout
boxplot(heat ~ method, data=dat, xlab="Method", ylab="Latent Heat (cal/gm)")
t.test(heat ~ method, data=dat)
by(dat$heat, dat$method, var)
var.test(heat ~ method, data=dat)
t.test(heat ~ method, data=dat, var.equal=TRUE)

plot(ecdf(dat$heat[dat$method=="A"]), do.points=FALSE, verticals=TRUE, xlim=range(A, B), col="red", lwd=5, main="")
plot(ecdf(dat$heat[dat$method=="B"]), do.points=FALSE, verticals=TRUE, add=TRUE, col="blue", lwd=5)
text(80.00, 0.77, "Method B", cex=1.5)
text(80.01, 0.25, "Method A", cex=1.5)

ks.test(heat ~ method, data=dat)

############################################################################

### 9.1: Grouped expressions

# like whatever ...

### 9.2: Control statements

x <- 5

if (x == 5) print("x is five!") else print("x is not five :(")

# note: the expression between () (the 'condition') must be a single TRUE or
# FALSE, so this does not work and generates an error
if (dat$heat > 80) print("heat is above 80") else print("heat is 80 or below")

## the difference between && and & (and similarly || and |)

# which values of 'heat' are larger than 80?
dat$heat > 80

# which values of 'method' are equal to A?
dat$method == "A"

# which values of 'heat' are larger than 80 and are from method A?
dat$heat > 80 & dat$method == "A"

# the double && is only for a single logical
y <- 7
x == 5 && y < 8

# this generates an error
dat$heat > 80 && dat$method == "A"

# why would you ever want to use &&?

# the && is evaluted left-to-right, so if the first logical is FALSE, then
# none of the following expressions are evaluated; for example, running
# mean(rnorm(10^10)) > 0 would not only take a huge amount of time, but would
# require more memory than my computer has; but running the following is no
# problem, because the second expression (mean(rnorm(10^10)) > 0) is never
# actually run
x == 4 && mean(rnorm(10^10)) > 0

# but do not run 'x == 4 & mean(rnorm(10^10)) > 0' because then both sides of
# the & are run/evaluated and this would crash my R session

# same thing with || (for 'or' comparisons); if the first expression is TRUE,
# then there is no need to evaluate the second and it isn't even run
x == 5 || mean(rnorm(10^10)) > 0

# the ifelse() function is a 'vectorized' version of if () else
ifelse(dat$heat > 80, "red", "blue")

# this is useful in all kinds of circumstances, for example to distinguish
# groups or values in plots; here is a silly example
plot(dat$heat, xlab="Observation Number", ylab="Heat", pch=19, cex=1.2,
     col=ifelse(dat$heat > 80, "red", "blue"))
