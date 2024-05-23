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

### 1: The Golem of Prague

# https://en.wikipedia.org/wiki/Fisher's_exact_test

#                      tea added first      milk added first
# "tea added first"          a                      b             n/2
# "milk added first"         c                      d             n/2
#                            n/2                    n/2           n

addmargins(table(mtcars$am, mtcars$vs))
chisq.test(table(mtcars$am, mtcars$vs))
fisher.test(table(mtcars$am, mtcars$vs))

addmargins(table(sleep$group, sleep$extra > 0))
chisq.test(table(sleep$group, sleep$extra > 0))
fisher.test(table(sleep$group, sleep$extra > 0))

res1 <- lm(mpg ~ 0 + factor(am), data=mtcars)
summary(res1)

library(nlme)

res2 <- lme(mpg ~ 1, random = ~ 1 | am, data=mtcars)
summary(res2)
coef(res2)

############################################################################

