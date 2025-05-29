############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-05-29
#
# Topic(s):
# - syntax style guides and syntax formatting
# - packages styler and lintr
# - the R formatter Air
#
# last updated: 2025-05-29

############################################################################

# there is no 'official' R style guide -- in the sense of that it can be found
# on the R Project website (https://www.r-project.org) or on CRAN
# (https://cran.r-project.org)

# but people have written such guides; some well-known syntax style guides:
# - https://style.tidyverse.org
# - https://google.github.io/styleguide/Rguide.html
# - https://contributions.bioconductor.org/r-code.html#r-code

# sometimes labs / research groups have their own style guide:
# - https://jef.works/R-style-guide/

############################################################################

# https://en.wikipedia.org/wiki/Lint_(software)

# - https://cran.r-project.org/package=styler
# - https://cran.r-project.org/package=lintr
# - https://cran.r-project.org/package=formatR
# - https://posit-dev.github.io/air/

############################################################################

# install the styler package and load it
#install.packages("styler")
library(styler)

# can style a file like the script from the last session (assuming it is in
# the current working directory); note: this changes the file, so make a
# backup if you want to keep the original
style_file("oor_stream_2025_05_22_code.r")

# in RStudio, there is an add-in that you can access via the toolbar (e.g.,
# Addins -> Style active file)

############################################################################

# install the lintr package and load it
#install.packages("lintr")
library(lintr)

# lint the same file; this shows all potential style issues, but does not
# actually change the file (not sure if that is even possible)
lint("oor_stream_2025_05_22_code.r")

############################################################################

# install the formatR package and load it
#install.packages("formatR")
library(formatR)

tidy_source("oor_stream_2025_05_22_code.r", indent=3, file="oor_stream_2025_05_22_code_formated.r")
tidy_file("oor_stream_2025_05_22_code.r", indent=3)



############################################################################
