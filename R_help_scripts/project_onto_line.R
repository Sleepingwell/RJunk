project <- function(x0, y0, a, b) {
    x1 <- (x0 + b*(y0-a)) / (b^2 + 1)
    c(x1, a + b*x1)
}


a <- 4
b <- 0.3

z0 <- cbind(runif(10, 0, 10), runif(10, 0, 10))
z1 <- t(mapply(project, z0[,1], z0[,2], MoreArgs=list(a=a, b=b)))

par(pty='s')
plot(z0[,1], z0[,2], xlim=c(0,10), ylim=c(0,10), pch=20)
points(z1[,1], z1[,2], col='red', pch=20)
arrows(z0[,1], z0[,2], z1[,1], z1[,2], length=0.1, col='red')
abline(a=a, b=b, col='red')
