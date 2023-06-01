library(juicr)
GUI_juicr(theFigureFile="figure_7_4.png", figureWindowSize=c(2000,1500), standardSize=2000)

sub <- read.csv("figure_7_3_juicr_extracted_points.csv")
sub$x <- round(sub$x.calibrated)
sub$y <- round(sub$y.calibrated, 4)
sub <- sub[order(sub$x),]
sub <- sub[c("x", "y")]
names(sub) <- c("age", "wage")
dput(sub)

dis <- rep(NA, nrow(sub))
pos <- rep(NA, nrow(sub))

for (i in 1:nrow(sub)) {

   dis[i] <- min(sqrt((sub$age[i] - dat$age)^2 + (sub$wage[i] - dat$wage)^2))
   pos[i] <- which.min(sqrt((sub$age[i] - dat$age)^2 + (sub$wage[i] - dat$wage)^2))

}

hist(dis)

dput(as.numeric(sort(pos)))
