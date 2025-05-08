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

## 6.3.1: Collider of false sorrow

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

## 6.3.2: The haunted DAG

# set values for the simulation
N <- 200  # number of grandparent-parent-child triads
b_GP <- 1 # direct effect of G on P
b_GC <- 0 # direct effect of G on C
b_PC <- 1 # direct effect of P on C
b_U  <- 2 # direct effect of U on P and C

# simulate the data
set.seed(1)
U <- 2*rbern(N, 0.5) - 1
G <- rnorm(N)
P <- rnorm(N, b_GP*G + b_U*U)
C <- rnorm(N, b_PC*P + b_GC*G + b_U*U)
dat <- data.frame(C=C, P=P, G=G, U=U)

# define and fit the model predicting C from P and G and inspect the results
res <- quap(alist(C ~ dnorm(mu, sigma),
                  mu <- a + b_PC*P + b_GC*G,
                  a ~ dnorm(0, 1),
                  c(b_PC,b_GC) ~ dnorm(0, 1),
                  sigma ~ dexp(1)), data=dat)
precis(res)

# Figure 6.5
sel <- P >= quantile(P, .45) & P <= quantile(P, .60)
plot(scale(C) ~ scale(G), pch=21, col=ifelse(U==1,"#1e59ae","black"),
     bg=ifelse(U==1,ifelse(sel,"#1e59ae","transparent"),ifelse(sel,"black","transparent")),
     xlab="gradparent education (G)", ylab="grandchild education (C)", bty="l")

# define and fit the model predicting C from P, G, and U and inspect the results
res <- quap(alist(C ~ dnorm(mu, sigma),
                  mu <- a + b_PC*P + b_GC*G + b_U*U,
                  a ~ dnorm(0, 1),
                  c(b_PC,b_GC,b_U) ~ dnorm(0, 1),
                  sigma ~ dexp(1)), data=dat)
precis(res)

############################################################################

### 6.4: Confronting confounding

## 6.4.2: Two roads

# load the dagitty package
library(dagitty)

# define and plot the DAG
dag <- dagitty("dag {U [unobserved] X -> Y X <- U <- A -> C -> Y U -> B <- C}")
plot(dag)

# check what variable(s) need to be adjusted to obtain an unbiased estimate of
# the relationship between X and Y
adjustmentSets(dag, exposure="X", outcome="Y")

# we can define exposure, outcome, and latent (unobserved) variables and their
# position when defining a DAG
dag <- dagitty("dag {
A [pos=\"-0.290,-1.324\"]
B [pos=\"-0.290,-0.644\"]
C [pos=\"0.815,-1.026\"]
U [latent,pos=\"-1.429,-1.026\"]
X [exposure,pos=\"-1.421,-0.130\"]
Y [outcome,pos=\"0.823,-0.130\"]
A -> C A -> U C -> B C -> Y U -> B U -> X X -> Y
}")
plot(dag)
adjustmentSets(dag)