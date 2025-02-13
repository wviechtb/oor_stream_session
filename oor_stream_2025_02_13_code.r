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

play <- function() {

   size1 <- 4
   size2 <- 4

   x1.pos.old <- rep(0.25, size1)
   y1.pos.old <- rep(0.25, size1)
   x1.pos.new <- x1.pos.old
   y1.pos.new <- y1.pos.old
   direction1 <- 3

   x2.pos.old <- rep(0.75, size2)
   y2.pos.old <- rep(0.75, size2)
   x2.pos.new <- x2.pos.old
   y2.pos.new <- y2.pos.old
   direction2 <- 1

   movesize <- 0.01

   points(x1.pos.new[1], y1.pos.new[1], pch=15, cex=1.5, col="black")
   points(x2.pos.new[1], y2.pos.new[1], pch=15, cex=1.5, col="red")
   Sys.sleep(2)

   x.block <- runif(1)
   y.block <- runif(1)

   points(x.block, y.block, pch=15, cex=1.5)

   idlefun <- function() {

      points(x.block, y.block, pch=15, cex=1.5)
      points(x1.pos.old[size1], y1.pos.old[size1], pch=15, col="white", cex=1.6)
      points(x2.pos.old[size2], y2.pos.old[size2], pch=15, col="white", cex=1.6)

      if (direction1 == 1)
         y.pos.new[1] <<- y.pos.old[1] - movesize
      if (direction1 == 2)
         x.pos.new[1] <<- x.pos.old[1] - movesize
      if (direction1 == 3)
         y.pos.new[1] <<- y.pos.old[1] + movesize
      if (direction1 == 4)
         x.pos.new[1] <<- x.pos.old[1] + movesize

      points(x.pos.new[1], y.pos.new[1], pch=15, cex=1.5)

      if (abs(x.pos.new[1] - x.block) <= 0.01 && abs(y.pos.new[1] - y.block) <= 0.01) {
         x.pos.new <<- c(x.pos.new, x.pos.new[size])
         y.pos.new <<- c(y.pos.new, y.pos.new[size])
         size <<- size + 1
         points(x.block, y.block, pch=15, col="white", cex=1.6)
         x.block <<- runif(1)
         y.block <<- runif(1)
      }

      x.pos.old <<- c(x.pos.new[1], x.pos.old[1:size-1])
      y.pos.old <<- c(y.pos.new[1], y.pos.old[1:size-1])

      return(NULL)

   }

   keyfun <- function(key) {

      if (identical(key, "Down"))
         direction <<- 1
      if (identical(key, "Left"))
         direction <<- 2
      if (identical(key, "Up"))
         direction <<- 3
      if (identical(key, "Right"))
         direction <<- 4

      if (identical(key, "q"))
         return(0)

      return(NULL)

   }

   click <- getGraphicsEvent(prompt="", onKeybd=keyfun, onIdle=idlefun)

}

par(mar=c(2,2,2,2))
plot(NA, xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", xaxt="n", yaxt="n")
play()
