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

for(tp in c("in", "dev", "ndc", "nfc", "npc", "nic", "lines", "chars"))
    print(grconvertX(c(0.5, 0.5), "user", tp))

while (TRUE) {

   fun.mousedown <- function(button,x,y) {
      return(NULL)
   }

   fun.mousemove <- function(button,x,y) {
      print(c(x,y))
   }

   fun.mouseup <- function(button,x,y) {
      return(NULL)
   }

   fun.key <- function(key) return(key)

   click <- getGraphicsEvent(prompt="", onMouseDown=fun.mousedown, onMouseMove=fun.mousemove,
                             onMouseUp=fun.mouseup onKeybd=fun.key)

   if (identical(click, "q"))
      break

   x <- grconvertX(click[1], from="ndc", to="user")
   y <- grconvertY(click[2], from="ndc", to="user")

   points(x, y, pch=19, cex=1)

}

