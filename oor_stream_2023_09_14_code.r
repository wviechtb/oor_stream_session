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