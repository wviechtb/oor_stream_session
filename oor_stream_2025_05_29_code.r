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

# install the lintr package and load it
#install.packages("lintr")
library(lintr)

