#--------------------------------------------------------------
# function to generate a contingency table that reflects
# a certain population
#
# parameters:
# n     The number of cases.
# b.pop The proportion of individuals getting a bad score before treatment.
# b2g   The probability that an individual whom gets a 0 before the test gets a 1 after the test.
# g2g   The probability that an individual whom gets a 1 before the test gets a 1 after the test.
#
# returns:
# A contingency table giving counts of cases that transition
# between the possible states (bad->bad, good->bad, bad->good and good->good)
# for the specified population.
#--------------------------------------------------------------
gen.pop <- function(n, b.pop, b2g, g2g) {
    #---------------------------------
    # check the parametes are valid
    #---------------------------------
    stopifnot(b.pop >= 0 && b.pop <= 1)
    stopifnot(b2g >= 0 && b2g <= 1)
    stopifnot(g2g >= 0 && g2g <= 1)

    #---------------------------------
    # data generation
    #---------------------------------
    n.b <- rbinom(1, n, b.pop)
    n.g <- n - n.b
    n.b.g <- rbinom(1, n.b, b2g)
    n.g.g <- rbinom(1, n.g, g2g)
    matrix(c(n.b-n.b.g, n.g-n.g.g, n.b.g, n.g.g), nrow=2)
}

#--------------------------------------------------------------
# McNemar's test (from http://en.wikipedia.org/wiki/McNemar's_test)
#
# Arguments:
# pop.table The contingency table.
# method    The test to use (see the wikipedia page above).
# NOTE:
# This test only tests the hypothesis that the training
# induces a change - that change could be positive or negative.
#--------------------------------------------------------------
mcnemar <- function(pop.table) {
    pchisq((pop.table[1,2] - pop.table[2,1])^2 / (pop.table[1,2] + pop.table[2,1]), 1, lower.tail=F)
}


#--------------------------------------------------------------
# 1) A little simulation of what happens with perfect sensitivity
#--------------------------------------------------------------
# trans The transition probabilities of: bad->bad, good->bad, bad->good and good->good (in that order).
n <- 1000
res <- t(sapply(1:n, function(x) {
    p <- runif(1) # the probability p1(-|+)
    pop <- gen.pop(225, 0.5, p, 1)
    c(p, mcnemar(pop))
}))

sp <- split(res[,2], cut(res[,1], breaks=seq(0, 1, 0.1), include.lowest=T))
cat('Proportion of tests that pass for bins of p1(+|-)\n')
sapply(sp, function(x) {
    x[is.na(x)] <- 1
    tmp <- sum(x < 0.05)
    tmp[is.na(tmp)] <- 0 
    tmp / length(x)
})
# uncomment if you can plot (can't on ideone.com
#par(mfrow=c(2,1))
#plot(res[,1], res[,2], xlab='p(+|-)', ylab='p-value', main='raw scale')
#plot(res[,1], log(res[,2]), xlab='p(+|-)', ylab='log(p-value)', main='log scale')

#--------------------------------------------------------------
# 2) the training is very damaging (should be a very small number)
#--------------------------------------------------------------
cat('\nthe training is very damaging (everyone whom takes the test gets 0 after the test\n')
mcnemar(gen.pop(225, 0.5, 0, 0))

#--------------------------------------------------------------
# 3) Sensitivity to specificity.
#
# This assumes perfect sensitivity, that the training has no value,
# but the but that the test still gives +ve results. It is very
# similar to simulation 1 above. The probability of a false
# +ve is a half normal with mean 0 and sd = 0.5
#--------------------------------------------------------------
n <- 1000
res <- t(sapply(1:n, function(x) {
    p <- abs(rnorm(1, 0, 0.05))
    pop <- gen.pop(225, 0.5, p, 1)
    c(p, mcnemar(pop))
}))

sp <- split(res[,2], cut(res[,1], breaks=seq(0, 0.2, 0.02), include.lowest=T))
cat('\nProportion of tests that pass for for various values of 1-sensitivity\n')
sapply(sp, function(x) {
    x[is.na(x)] <- 1
    tmp <- sum(x < 0.05)
    tmp[is.na(tmp)] <- 0 
    tmp / length(x)
})
# uncomment if you can plot (can't on ideone.com
#dev.off()
#plot(res[,1], res[,2], xlab='p(+|-)', ylab='p-value', main='raw scale', sub=paste('error rate:', sum(res[,2] < 0.05)/n))

#--------------------------------------------------------------
# 4) Affect of proportion of population that will pass fail
# before training
#
# Assumes the test is not damaging, perfect sensitivity and specificity,
# and that the test will help 50% of people who score 0 without training.
#
# It would be desirable if the result did not depend on this proportion.
#--------------------------------------------------------------
n <- 1000
res <- t(sapply(1:n, function(x) {
    p <- runif(1)
    pop <- gen.pop(225, p, 0.99, 1)
    c(p, mcnemar(pop))
}))

sp <- split(res[,2], cut(res[,1], breaks=seq(0, 1, 0.1), include.lowest=T))
cat('\nAffect of proportion of population that fail without training\n')
sapply(sp, function(x) {
    x[is.na(x)] <- 1
    tmp <- sum(x < 0.05)
    tmp[is.na(tmp)] <- 0 
    tmp / length(x)
})
# uncomment if you can plot (can't on ideone.com
#dev.off()
#plot(res[,1], res[,2], xlab='p(+|-)', ylab='p-value')








#--------------------------------------------------------------
# binomial tests:
#--------------------------------------------------------------
n <- 10000
res <- t(sapply(10:n, function(x) {
    p <- runif(1)
    pop <- gen.pop(225, 0.5, p, 1)
    n <- sum(pop[1,])
    p.est <- pop[1,2] / n
    c(p, p.est, qbinom(0.025, n, p.est)/n, qbinom(0.975, n, p.est)/n)
}))
cat('\ncheck the coverage\n')
sum(res[,1] >= res[,3] & res[,1] <= res[,4])/(n-9)

























#--------------------------------------------------------------
# McNemar's test (from http://en.wikipedia.org/wiki/McNemar's_test)
#
# Arguments:
# pop.table The contingency table.
# method    The test to use (see the wikipedia page above).
# NOTE:
# This test only tests the hypothesis that the training
# induces a change - that change could be positive or negative.
#--------------------------------------------------------------
mcnemar <- function(pop.table, method=c('orig', 'yates', 'edwards')) {
    method <- match.arg(method)
    num <- pop.table[1,2] - pop.table[2,1]
    den <- pop.table[1,2] + pop.table[2,1]
    if(den < 25) warning('Chi-sqared test may not be that good')
    num <- if(method == 'orig') {
        num
    } else if(method == 'yates') {
        abs(num) - 0.5
    } else { # then method == 'edwards'
        abs(num) - 1
    }
    if(den == 0) {
        warning('treatment has no effect at all!')
        1
    } else {
        pchisq(num^2 / den, 1, lower.tail=F)
    }
}



#--------------------------------------------------------------
# binomial (exact) test
#
# parameters:
# test:
# - 'type1'     The training has some effect (either positive or negative).
# - 'type2'     The training has a positive effect, given that it has some effect.
#--------------------------------------------------------------
binom <- function(pop.table, test=c('type1', 'type2') {
    test <- match.arg('test')
    if(test == 'type1') {
        stat <- pop.table[1,2] + pop.table[2,1]
        n <- sum(pop.table)
        structure(
            c(t=stat, p=pbinom(stat, n, 0.5, F)),
            n=n
        )
    } else if(test == 'type2') {
        stat <- pop.table[1,2]
        n <- sum(pop.table[1,2] + pop.table[2,1]
        structure(
            c(t=stat, p=pbinom(stat, n, 0.5, F)),
            n=n
        )
    }
}
