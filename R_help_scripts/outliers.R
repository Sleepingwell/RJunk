x <- c(1,9,1,9,1,9,1,9,5)
# in this case, we don't need to know anything about the 'timeseriesness' of the data
# find the least common elements
find.uncommon1 <- function(x, n=length(x)/5.0) {
    res <- table(x)
    vals <- as.numeric(names(res))
    inds <- order(res)
    res <- cumsum(res[inds])
    as.numeric(vals[inds][res <= n])
}
find.uncommon1(x)

# or we could base it on a density (could take differences
# of x of some order to find unusual values in a 'time series').
find.uncommon2 <- function(x, n=1, bw=1, kernel="rectangular") {
    prs <- approxfun(density(x, bw=1, kernel=kernel))(x)
    x[order(prs)][n]
}
find.uncommon2(x)

# find elements that are a "much" more uncommon that the next most uncommon
find.uncommon3 <- function(x, bw=0.5) {
    res <- table(x)
    vals <- as.numeric(names(res))
    inds <- order(res)
    res <- c(diff(res[inds])/length(x),0)
    diff.frac <- bw * diff(range(res))
    as.numeric(vals[inds][res > diff.frac])
}
find.uncommon3(x)


    
library(wavelets)
x <- c(c(rep(c(1,9), 50)), 5)
x <- c(rnorm(100, rep(c(1,9), 50)), 5)
x <- c(1,9,1,9,1,9,1,9,5)
x.w <- dwt(x, fast=F)

idwt(dwt(x))
idwt(dwt(x, fast=F), fast=F)
plot(x.w)

res <- density(x, bw=1, kernel="rectangular", give.Rkern=T)

x[
