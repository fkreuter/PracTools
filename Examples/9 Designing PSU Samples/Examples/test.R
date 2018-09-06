
wts <- abs(rnorm(54,2,1))
warpbreaks <- cbind(warpbreaks, wts)
with(warpbreaks,
            by(warpbreaks, tension,
               function(x) lm(breaks ~ wool, data = x, weights=wts)))

wtdvar <- function(x, w){
    xbarw <- sum(w*x) / sum(w)
#    xbarw <- weighted.mean(x, w)
    varw <- sum(w * (x-xbarw)^2) / sum(w)
    varw
}

wtdvar(x=dat$x,w=dat$wt)

dat <- data.frame(psu=c(1,1,2,2), wt=1:4, x=c(1,3,5,7))
by(dat$x, INDICES=dat$psu, FUN=wtdvar, w=dat$wt)


summaryBy(x~psu, data=dat, FUN=wtdvar, w=wt)
summaryBy(x~psu, data=dat, FUN=wtdvar(w=wt))
