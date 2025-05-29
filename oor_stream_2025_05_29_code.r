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

# - https://cran.r-project.org/package=styler
# - https://cran.r-project.org/package=lintr
# - https://posit-dev.github.io/air/

x = c(2, 4, 3, 5, 6)
x <- c(2, 4, 3, 5, 6)

dat <- data.frame(species = c("afarensis", "africanus", "habilis", "boisei", "rudolfensis", "ergaster", "sapiens"),
                  pos     = c(4, 3, 3, 4, 4, 3, 1),
                  brain   = c(438, 452, 612, 521, 752, 871, 1350),
                  mass    = c(37.0, 35.5, 34.5, 41.5, 55.5, 61.0, 53.5))
