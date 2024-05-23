############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-05-23
#
# Topic(s):
# - Statistical Rethinking (https://xcelab.net/rm/)
# - Section(s): 1.1 - ?
#
# last updated: 2024-05-23

############################################################################

# install rstan package (note: need to install a C++ compiler for this to
# work; for details, see: https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started)
install.package("rstan")

# install some additional packages needed
install.packages(c("coda","mvtnorm","remotes","dagitty"))

# install the cmdstanr package
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

# install CmdStan via the cmdstanr package
cmdstanr::install_cmdstan()

# install the rethinking package
remotes::install_github("rmcelreath/rethinking")




############################################################################
