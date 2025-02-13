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

   # set some defaults
   col <- "black"
   mode <- "line"

   fun.mousedown <- function(button,x,y) {
      pressed <<- TRUE
      x <- grconvertX(x, from="ndc", to="user")
      y <- grconvertY(y, from="ndc", to="user")
      x.last <<- x
      y.last <<- y
      x.box.start <<- x
      y.box.start <<- y
      return(NULL)
   }

   fun.mousemove <- function(button,x,y) {
      if (pressed) {
         x <- grconvertX(x, from="ndc", to="user")
         y <- grconvertY(y, from="ndc", to="user")
         if (mode == "line")
            segments(x.last, y.last, x, y, lwd=ifelse(col=="white",30,4), col=col)
         x.last <<- x
         y.last <<- y
      }
      return(NULL)
   }

   fun.mouseup <- function(button,x,y) {
      pressed <<- FALSE
      if (mode == "box")
         rect(x.box.start, y.box.start, x.last, y.last, lwd=5, border=col)
      return(NULL)
   }

   fun.key <- function(key) return(key)

   while (TRUE) {

      pressed <- FALSE
      x.last <- NA_real_
      y.last <- NA_real_
      x.box.start <- NA_real_
      y.box.start <- NA_real_

      click <- getGraphicsEvent(prompt="", onMouseDown=fun.mousedown,
                                onMouseMove=fun.mousemove,
                                onMouseUp=fun.mouseup, onKeybd=fun.key)

      # <r> key to switch color to red
      if (identical(click, "r"))
         col <- "red"

      # <b> key to switch color to black
      if (identical(click, "b"))
         col <- "black"

      # <e> key to switch to eraser
      if (identical(click, "e")) {
         col <- "white"
         mode <- "line"
      }

      # <m> key to toggle between line and box drawin mode
      if (identical(click, "m"))
         mode <- ifelse(mode == "line", "box", "line")

      # <q> key to quit
      if (identical(click, "q"))
         break

   }

}

dat <- mtcars
plot(dat$hp, dat$mpg, pch=21, bg="gray", cex=1.5)
draw()

############################################################################

draw <- function() {

   x.pos <- 0.5
   y.pos <- 0.5
   direction <- 3

   idlefun <- function() {

   }

   while (TRUE) {

      click <- getGraphicsEvent(prompt="", onKeybd=function(key) return(key), onIdle=idlefun)

      if (identical(click, "Down"))
         direction <- 1
      if (identical(click, "Left"))
         direction <- 2
      if (identical(click, "Up"))
         direction <- 3
      if (identical(click, "Right"))
         direction <- 4

   }

}

par(mar=c(2,2,2,2))
plot(NA, xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", xaxt="n", yaxt="n")
