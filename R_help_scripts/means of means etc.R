library(splinesurv)
library(JM)
s <- sim.sample(m = 2, Ji = rep(10,2), params = list(haz.type = "weibull", haz.params = list(lambda0 = 1 , gamweib = 1), frail.type = "weibull", frail.params = list(lambda0 = 1 , gamweib = 1))) 
fit <- weibull.frailty(Surv(time, delta) ~ Z + frailty(i), data = s$agdata, id="i") 
summary(s)

# I presume you say...
library(splinesurv)
library(JM)
# somewhere?





#1a)
# first, consider the each i separately
# what one might do for a single variable:
mns <- with(s$agdata, c(tapply(time, i, mean), tapply(Z, i, mean)))
sds <- with(s$agdata, c(tapply(time, i, sd), tapply(Z, i, sd)))

# When you see the same code like this,
# you should think "Ah! sapply is my friend"
# and try something like one of the following
with(s$agdata, sapply(list(time=time, Z=Z), function(x) c(mean=tapply(x, i, mean), sd=tapply(x, i, sd))))
with(s$agdata, lapply(list(time=time, Z=Z), function(x) c(mean=tapply(x, i, mean), sd=tapply(x, i, sd))))
lapply(split(s$agdata[,c('time', 'Z')], s$agdata$i), apply, 2, function(x) c(mean=mean(x), sd=sd(x)))

# depending what you want as output.




#1b)
apply(s$agdata[,c('time', 'Z')], 2, function(x) c(mean=mean(x), sd=sd(x)))




# 2)
# I'm not sure I understand the question,
# but if you are wanting to generate
# random datasets 10 time, make the RHS
# an expression and evaluate it in a loop
# for the AIC and BIC, you'd say:
s.exp <- expression(sim.sample(m = 2, Ji = rep(10,2), params = list(haz.type = "weibull", haz.params = list(lambda0 = 1 , gamweib = 1), frail.type = "weibull", frail.params = list(lambda0 = 1 , gamweib = 1))))
lapply(1:10, function(x) {
    s <- eval(s.exp)
    fit <- weibull.frailty(Surv(time, delta) ~ Z + frailty(i), data=s$agdata, id="i") 
    with(summary(fit), c(AIC, BIC))
})
# modify this by returning whatever is necessary




# 3)
# depends on which version of the code you use
# version 1 is probably the easiest, then:
res <- with(s$agdata, sapply(list(time=time, Z=Z), function(x) c(mean=tapply(x, i, mean), sd=tapply(x, i, sd))))
mns.mns <- apply(res[1:2,], 2, mean)
