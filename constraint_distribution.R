#-------------------------------------------------------------------------------------------
# code to look at the joint distribution of areas.
#-------------------------------------------------------------------------------------------
joint <- function(N, N1, N2, N3, S1, S2, S3, L1, L2, L3){
    a1 <- choose(L1, S1)
    a2 <- choose(L2, S2)
    a3 <- choose(L3, S3)
    b1 <- choose(N-L1-S2-S3, L1-S1)
    b2 <- choose(N-L2-S1-S3, L2-S2)
    b3 <- choose(N-L3-S1-S2, L3-S3)
    a1*a2*a3*b1*b2*b3/choose(N, N1+N2+N3)
}
    
    
tester <- function(a1, a2, a3, size=500){
    z1 <- sample(0:1, size=500, replace=T)
    z2 <- sample(0:1, size=500, replace=T)
    z3 <- as.numeric(z1 & z2)
    z1 <- as.numeric(z1 & !z3)
    z2 <- as.numeric(z2 & !z3)
}

tester <- function(n, l1, l2, s1, s2){
    a <- choose(l1, s1) * choose(l2, s2) * choose(n-l1-s2, l1-s1) * choose(n-l2-l1, l2-s2)
    b <- choose(l1, s1) * choose(l2, s2) * choose(n-l2-s1, l2-s2) * choose(n-l2-l1, l1-s1)
    c(a, b)
}

tester(100, 33, 47, 22, 35)
