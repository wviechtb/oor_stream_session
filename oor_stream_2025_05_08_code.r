############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-05-08
#
# Topic(s):
# - Statistical Rethinking (https://xcelab.net/rm/)
# - Section(s): 6.3 - ?
#
# last updated: 2025-05-08

############################################################################

# load the rethinking package
library(rethinking)

############################################################################

### 6.3: Collider bias

# simulate data according to the age, marriage, and happiness example
d <- sim_happiness(seed=1977, N_years=1000)
precis(d)

# inspect the code for sim_happiness() to see how exactly the data are simulated
sim_happiness
