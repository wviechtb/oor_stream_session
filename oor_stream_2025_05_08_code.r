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
dat <- sim_happiness(seed=1977, N_years=1000)
precis(dat)

# inspect the code for sim_happiness() to see how exactly the data are simulated
sim_happiness

# Figure 6.4: plot of age versus happiness with blue dots corresponding to
# married individuals
plot(happiness ~ age, data=dat, pch=21, bg=ifelse(married==1, "#1e59ae", "white"), bty="l")
legend(16, 2.7, pch=21, pt.bg=c("white","#1e59ae"), legend=c("unmarried","married"),
       horiz=TRUE, xpd=TRUE, text.width=20, bty="n")

# make a copy of data that only incldues the adults (age >= 18)
dat2 <- dat[dat$age >= 18,]

# rescale the age variable so 0 corresponds to 18 and 1 corresponds to 65
dat2$A <- (dat2$age - 18) / (65 - 18)

# marriage status indicator (1 = not married, 2 = married)
dat2$mid <- dat2$married + 1

# define the model predicting happiness from marriage status and A (age)
model <- alist(happiness ~ dnorm(mu, sigma),
               mu <- a[mid] + bA*A,
               a[mid] ~ dnorm(0, 1),
               bA ~ dnorm(0, 2),
               sigma ~ dexp(1))

# fit the model and inspect the results
res <- quap(model, data=dat2)
precis(res, depth=2)

# define the model without marraige status as predictor
model <- alist(happiness ~ dnorm(mu, sigma),
               mu <- a + bA*A,
               a ~ dnorm(0, 1),
               bA ~ dnorm(0, 2),
               sigma ~ dexp(1))

# fit the model and inspect the results
res <- quap(model, data=dat2)
precis(res, depth=2)

# there is no correlation between A (age) and happiness
cor(dat2$A, dat2$happiness)

# but within each subgroup (unmarried and married), there is negative
# correlation between A (age) and happiness
cor(dat2$A[dat2$mid == 1], dat2$happiness[dat2$mid == 1])
cor(dat2$A[dat2$mid == 2], dat2$happiness[dat2$mid == 2])

