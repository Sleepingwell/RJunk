library(foreach)
library(iterators)
nr <- 10
nc <- 5
matrix1 <- matrix(rnorm(nr*nc), nrow=nr)
matrix2 <- matrix(sample(3,nr*nc,T), nrow=nr)
foreach(x=iter(matrix1, by='col'), ind=iter(matrix2, by='col'), .combine='c') %dopar% x[ind==1] # should be same as dat[inds==1]
foreach(x=iter(matrix1, by='col'), ind=iter(matrix2, by='col'), .combine='cbind', .inorder=T, .multicombine=T) %dopar% {x[ind==1] <- "result";x} 
