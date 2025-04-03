############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-04-03
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 12.7 - ?
#
# last updated: 2025-04-03

############################################################################

# load the rstanarm package
library(rstanarm)

############################################################################

### 12.7: Models for regression coefficients

# download the dataset if it doesn't already exist
if (!file.exists("student-merged.csv")) download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/refs/heads/master/Student/data/student-merged.csv", destfile="student-merged.csv")

# read in the dataset
dat <- read.csv("student-merged.csv")

# inspect the first six rows of the dataset
head(dat)

# set up a vector with the names of the predictor variables
predictors <- c("school","sex","age","address","famsize","Pstatus","Medu",
                "Fedu", "traveltime","studytime","failures","schoolsup",
                "famsup", "paid","activities","nursery","higher","internet",
                "romantic","famrel","freetime","goout","Dalc","Walc","health",
                "absences")

# select rows where the final-year mathematics grade (G3mat) is > 0 and only
# select this variable plus the predictors
dat <- subset(dat, subset=G3mat>0, select=c("G3mat",predictors))
head(dat)

# predict G3mat from all other variables in the dataset
res0 <- stan_glm(G3mat ~ ., data=dat)

