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

# if components have names, can use those to select components
Lst[["no.children"]]
Lst$no.children

# sidenote: with $, we can abbreviate component names, but this does not work
# with the [["component_name"]] notation
Lst$no.c
Lst[["no.c"]]

# NULL means that there is no list element of that name

# the [[""]] notation also allows us to use variables for the variable names
x <- "name"
Lst[[x]]

# with $ notation, this is not possible; this will try to get the list element
# called 'x' (and not use what is stored in 'x' as component name)
Lst$x

# we can also use [] on lists; for example, the following will return a list
# which contain a single component, namely the first component
Lst[1]

# why does this distinction matter? for example, we can take the mean of a
# numeric vector, but we cannot take the mean of a list (even if that list
# only has a single component)
mean(Lst[["child.ages"]])

# so this does not work (trying to take the mean of a list)
mean(Lst["child.ages"])

# accordingly, [] notation allows extracting multiple list elements from a
# list; for example, this will return a list with components 1 and 3 from 'Lst'
Lst[c(1,3)]

