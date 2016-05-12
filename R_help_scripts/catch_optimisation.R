nfleets<-2
M<-1
M<-array(M,dim=c(nfleets))
N<-1000
cost<-c(30,30)
cost<-array(cost,dim=c(nfleets))
Price<-2
Price<-array(Price,dim=c(nfleets))
q<-array(0.1,dim=c(nfleets))
f<-1
f<-array(f,dim=c(nfleets))
f1<-f[1]
f2<-f[2]
init.eff<-array(8,dim=c(nfleets))
OF<-c(q*f)
F<- sum(q*f)
Z<-M+F
Catch<-array(0,dim=c(nfleets))

obj<-function(f){
    F <- q*f
    Z <- M+sum(F)
    S <- exp(-Z)
    Catch <- N*F/Z*(1-S)
    Tot.Catch <- sum(Catch)
    NR <- array(0,dim=c(nfleets))
    NR <- Price*Catch - f*cost
    d.NR <- array(0,dim=c(nfleets))
    browser()
    d.NR <- N*q/Z*(1-S-F/Z+F/Z*S+F*S)*Price - cost + 1000*(max(0,f-9))^2
    return(sum(d.NR*d.NR))
}

zero.bnd <-  rep.int(0, length(f))
opt.eff  <- optim(init.eff, obj, method="L-BFGS-B", lower=zero.bnd)











nfleets<-2
nareas<-2
M<-1
M<-array(M,dim=c(nfleets,nareas))
N<-1000
cost<-c(30,30)
cost<-array(cost,dim=c(nfleets,nareas))
Price<-2
Price<-array(Price,dim=c(nfleets,nareas))
q<-array(0.1,dim=c(nfleets,nareas))
f<-1
f<-array(f,dim=c(nfleets,nareas))
init.eff<-array(3,dim=c(nfleets,nareas))
OF<-array(c(q*f), dim=c(nfleets, nareas))
F<- sum(q*f)
Z<-M+F
Catch<-array(0,dim=c(nfleets, nareas))

obj<-function(f){
    f <- array(f, dim=c(nfleets, nareas))
    F <- q*f
    Z <- M+sum(F)
    S <- exp(-Z)
    Catch<- N*F/Z*(1-S)
    Tot.Catch <- sum(Catch)
    NR<-array(0,dim=c(nfleets,nareas))
    NR<-Price*Catch - f*cost
    d.NR<-array(0,dim=c(nfleets,nareas))
    f <- apply(f, 1, sum) #sum f in each 
    d.NR<- N*q/Z*(1-S-F/Z+F/Z*S+F*S)*Price - cost + 1000*max(0,(f-9))^2
    return(sum(d.NR*d.NR))
}

zero.bnd <-  rep.int(0, length(f))
opt.eff  <- optim( init.eff, obj, method="Nelder-Mead" )
