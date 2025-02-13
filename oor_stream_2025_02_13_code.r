############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-02-13
#
# Topic(s):
# - the 'chesstrainer' package
# - interactive apps (without Shiny)
#
# last updated: 2025-02-13

############################################################################

### the chesstrainer package

# https://github.com/wviechtb/chesstrainer
# https://wviechtb.github.io/chesstrainer/

# install the package
install.packages("remotes")
remotes::install_github("wviechtb/chesstrainer")

# load the package
library(chesstrainer)

# start playing
play()

############################################################################

plot(1)

while (TRUE) {

   pressed <- FALSE

   fun.mousedown <- function(button,x,y) {
      pressed <<- TRUE
      return(NULL)
   }

   fun.mousemove <- function(button,x,y) {
      if (pressed) {
         x <- grconvertX(x, from="ndc", to="user")
         y <- grconvertY(y, from="ndc", to="user")
         points(x, y, pch=19, cex=1)
      }
      return(NULL)
   }

   fun.mouseup <- function(button,x,y) {
      pressed <<- FALSE
      return(NULL)
   }

   fun.key <- function(key) return(key)

   click <- getGraphicsEvent(prompt="", onMouseDown=fun.mousedown, onMouseMove=fun.mousemove,
                             onMouseUp=fun.mouseup, onKeybd=fun.key)

   if (identical(click, "q"))
      break


}

