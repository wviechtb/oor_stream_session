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

draw <- function() {

   col <- "black"

   while (TRUE) {

      pressed <- FALSE
      x.last <- NA_real_
      y.last <- NA_real_

      fun.mousedown <- function(button,x,y) {
         pressed <<- TRUE
         x <- grconvertX(x, from="ndc", to="user")
         y <- grconvertY(y, from="ndc", to="user")
         x.last <<- x
         y.last <<- y
         return(NULL)
      }

      fun.mousemove <- function(button,x,y) {
         if (pressed) {
            x <- grconvertX(x, from="ndc", to="user")
            y <- grconvertY(y, from="ndc", to="user")
            #points(x, y, pch=19, cex=0.5)
            segments(x.last, y.last, x, y, lwd=ifelse(col=="white",30,4), col=col)
            x.last <<- x
            y.last <<- y
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

      if (identical(click, "r"))
         col <- "red"

      if (identical(click, "b"))
         col <- "black"

      if (identical(click, "e"))
         col <- "white"

      if (identical(click, "q"))
         break

   }

}

dat <- mtcars
plot(dat$hp, dat$mpg, pch=21, bg="gray", cex=1.5)
draw()
