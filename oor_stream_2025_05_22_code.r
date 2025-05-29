############################################################################

# load the rethinking package
library(rethinking)

############################################################################

### 7.1: The problem with parameters

## 7.1.1: More parameters (almost) always improve fit

# create the small dataset
dat <- data.frame(species = c(
  "afarensis", "africanus", "habilis", "boisei",
  "rudolfensis", "ergaster", "sapiens"
), pos = c(4, 3, 3, 4, 4, 3, 1), brain = c(
  438,
  452, 612, 521, 752, 871, 1350
), mass = c(
  37, 35.5, 34.5, 41.5, 55.5, 61,
  53.5
))

############################################################################
