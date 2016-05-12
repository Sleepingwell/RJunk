library(MASS)

dat <- data.frame(
    col1=as.factor(sample(1:4, 100, T)),
    col2=as.factor(sample(1:4, 100, T)),
    col3=as.factor(sample(1:4, 100, T)),
    isi=rnorm(100)
)

dat <- split(dat, as.factor(sample(1:3, 100, T)))
lapply(dat, function(x, densfun) fitdistr(x$isi, densfun), 'normal')
