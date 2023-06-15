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

##  6.2.1 Concatenating lists

# combine/concatenate a bunch of lists into a new list
c(Lst, z, dat)

############################################################################

### 6.3: Data frames

## 6.3.1: Making data frames

# combine the 4 variables created earlier into a data frame
dat <- data.frame(id, age, sex, grp)
dat

# remove the original vector objects
rm(id, age, sex, grp)

# data frames are of class 'data.frame'
class(dat)

# internally, data frames are stored as lists; we can see this if unclass 'dat'
unclass(dat)

# can index rows and columns using [] notation

# give me the first row from dat
dat[1,]

# give me rows 1 and 3 from dat
dat[c(1,3),]

# give me the second column from dat
dat[,2]

# can also refer to columns by their variable names
dat[,"age"]

# since data frames are lists, we can also use [[]] and $ and [] notation
dat[["age"]]
dat$age
dat["age"]
dat[c("age","grp")]

# note: this is returning the second column from dat as a vector

# give me columns 2 and 4 from dat
dat[,c(2,4)]

# with drop=FALSE, a single column from dat stays a data frame
dat[,2,drop=FALSE]

## 6.3.2: attach() and detach()

# this will give an error, because R does not find 'age' in 'dat'
age

# as shown above, we can use $ notation to access variables within data frames
dat$age

# the 'search paths' are the locations where R looks for things
searchpaths()

# attach 'dat' to the search paths
attach(dat)

# object 'dat' is now part of where R looks for things
searchpaths()

# now we can refer to elements from 'dat' directly
age

# remove 'dat' from the search paths
detach(dat)

# check what the search paths are
searchpaths()

# add a few more variables to dat
dat$y1 <- c(2,5,5)
dat$y2 <- c(3,2,4)
dat$y3 <- c(5,4,6)
dat

# compute the sum score of variables y1, y2, y3
dat$y1 + dat$y2 + dat$y3

# could use attach() to make this computation more convenient
attach(dat)
y1 + y2 + y3
detach(dat)

# add this sum score as a new variable to the dataset
dat$ysum <- dat$y1 + dat$y2 + dat$y3
dat

# now do the same thing using the attach() workflow
dat$ysum <- NULL

# might try this
attach(dat)
ysum <- y1 + y2 + y3
detach(dat)
dat

# uhm, where is ysum?!? what we have done is create a vector called 'ysum' in
# our workspace, but it is not part of 'dat'
ls()
ysum

# have to still tell R that we want ysum to be a new variable inside dat
rm(ysum)
attach(dat)
dat$ysum <- y1 + y2 + y3
detach(dat)
dat

# while using attach() can be convenient, it can also be confusing; say we
# have an object in our workspace called y1 (in addition to a variable called
# y1 inside of dat)
y1 <- c(100, 200, 500)

# now attach 'dat' and compute the sum of y1, y2, y3
attach(dat)
dat$ysum <- y1 + y2 + y3
detach(dat)
dat

# remove ysum from 'dat' again
dat$ysum <- NULL

# as we can see, the sum was created based on y1 from the workspace (the
# 'global environment') and y2 and y3 from 'dat'; this can be confusing and
# lead to errors; this is why some people discourage the use of attach()

# instead of using attach(), we can use with()
dat$ysum <- with(dat, y1 + y2 + y3)
dat

## 6.3.3: Working with data frames

# this is suggesting using attach(), so I would say we can skip this advice

# one piece of advice though: when creating new variables (e.g., like a sum
# score as shown above), make sure to add the variable to the data frame and
# not put it into your global environment; in other words, do not do this
ysum <- with(dat, y1 + y2 + y3)

# now there is a ysum vector floating around in the workspace that is
# independent from the data frame, which can easily lead to errors; for
# example, if we remove some cases from 'dat', then this has no affect on ysum
dat <- dat[dat$sex == "Male",]
dat
ysum

## 6.3.4: Attaching arbitrary lists

# again, do NOT use attach() (unless you know exactly what you are doing)

## 6.3.5: Managing the search path

# shows the current search paths
search()
searchpaths()

############################################################################
