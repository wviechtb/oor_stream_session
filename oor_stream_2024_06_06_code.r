############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-06-06
#
# Topic(s):
# - Statistical Rethinking (https://xcelab.net/rm/)
# - Section(s): 2.1 - ?
#
# last updated: 2024-06-06

############################################################################

obs <- c("B","W","B")

conj <- expand.grid(replicate(3, c("B","W","W","W"), simplify=FALSE))
conj[conj[,1] == obs[1] & conj[,2] == obs[2] & conj[,3] == obs[3],]

conj <- expand.grid(replicate(3, c("B","B","W","W"), simplify=FALSE))
conj[conj[,1] == obs[1] & conj[,2] == obs[2] & conj[,3] == obs[3],]

conj <- expand.grid(replicate(3, c("B","B","W","W"), simplify=FALSE))

sel <- conj
for (i in 1:3) {
   sel <- sel[sel[,i] == obs[i],]
}
sel

