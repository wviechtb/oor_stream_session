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
Lst[c("name", "no.children")]

# analogously, [[]] notation cannot be used extract multiple list elements
Lst[[c(3,4)]]

# an example of a list that does not have component names
z <- list(c(1,4,6), "Chicken", diag(4))
z

# can also have list where some components have names while others do not
z <- list(c(1,4,6), animal="Chicken", diag(4))
z

# sidenote: while using abbreviated component names with the $ notation seems
# convenient, it is also a potential source of errors or confusion; we can
# instruct R to give us a warning whenever abbreviated component names are used
options(warnPartialMatchDollar=TRUE)
Lst$child

# get the attributes of 'Lst' (which here includes the component names)
attributes(Lst)

############################################################################

### 6.2: Constructing and modifying lists

# an example showing how to create a list from existing objects
id  <- c("Bob", "Sue", "John")
age <- c(25, 21, 30)
sex <- c("Male", "Female", "Male")
grp <- c("Trt", "Trt", "Ctrl")
dat <- list(id, age, sex, grp)
dat

# the objects that are put into the list are copied, so changing one of the
# original objects does not affect the list
age <- c(32, 26, 18)
dat

# add a fifth element to the list
dat[[5]] <- c(7,3,5)
dat

# can also use the component name between [[]]
dat[["whatever"]] <- c(1,4,2)
dat

# or again use $ notation
dat$blah <- c(4,1,5)
dat
