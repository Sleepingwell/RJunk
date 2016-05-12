n <- 1000
b1 <- 1
b2 <- -1
x1 <- sample(0:1, n, T, c(0.1, 0.9))
x2 <- sample(0:1, n, T, c(0.1, 0.9))

y <- b1*x1 + b2*x2 + rnorm(n)

mod1 <- lm(y~x1+x2)
summary(mod1)

x3 <- ifelse(x1 | x2, 1, 0)

mod2 <- lm(y~x3)
summary(mod2)
