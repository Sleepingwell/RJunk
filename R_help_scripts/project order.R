#The assumptions (I may have misunderstood 4 and 5 - note however, that this does not affect the choice of strategy):
#1. project outcomes are independent.
#2. at least 1 project is attempted.
#3. one will try a second project if and only if the first is successful.
#4. the return on A is 3000 + 2000 (if I'm understanding what you mean by "over cost").
#5. the return on B is 5000 + 2000 (if I'm understanding what you mean by "over cost").

# values
pa <- 0.7 # pr success of a
pb <- 0.4 # pr success of b
ra <- 5000 # return on a
rb <- 7000 # return on b
ca <- cb <- -2000 # initial outlays

# function for moments
# - c1 is the cost of starting the first project,
# - c2 is the cost of starting the second project,
# - p1 is the probability of success of the first project,
# - p2 is the probability of success of the second project,
# - r1 is the return on success of the first project, and
# - r2 is the return on success of the second project
# - p is order of moment to calculate.
m <- function(c1, p1, r1, c2, p2, r2, p) {
    (1-p1)*c1^p + # first project fails (second never attempted)
    p1*(1-p2)*(c1+r1+c2)^p + # first succeeds and second fails
    p1*p2*(c1+r1+c2+r2)^p # first succeeds and second succeeds
}

# first moments (means)
e1a <- m(ca, pa, ra, cb, pb, rb, 1)
e1b <- m(cb, pb, rb, ca, pa, ra, 1)

# second moments
e2a <- m(ca, pa, ra, cb, pb, rb, 2)
e2b <- m(cb, pb, rb, ca, pa, ra, 2)

# variances
va <- e2a - e1a^2
vb <- e2b - e1b^2

# summary
c(e1a, sqrt(va))
c(e1b, sqrt(vb))
