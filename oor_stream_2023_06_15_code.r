############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-06-15
#
# Topic(s):
# - An Introduction to R
#   https://cran.r-project.org/doc/manuals/r-release/R-intro.html
# - Section(s): 6.1 - ?
#
# last updated: 2023-06-15

############################################################################

### 6.1: Lists

# create an example of a list
Lst <- list(name="Fred", wife="Mary", no.children=3, child.ages=c(4,7,9))
Lst

# can always refer to list elements by their number
Lst[[1]]
Lst[[4]]

# if a component of a list is a vector, can use [] notation to subset its elements
Lst[[4]][1]

# the length of a list is the number of components
length(Lst)
