# a little sample of how one might make a matrix that allows for views (or references).
library(proto)

my.mat <- function(mat) {
    structure(proto( expr = {
        mat <- mat
        set.vals <- function(., rs, cs, vals) {
            if(max(rs) > nrow(.$mat) || min(rs) < 1) stop("row out of range")
            if(max(cs) > ncol(.$mat) || min(cs) < 1) stop("column out of range")
            .$mat[rs, cs] <- vals
        }  
        get.mat.ref <- function(., rows, cols) {
            if(max(rows) > nrow(.$mat) || min(rows) < 1) stop("row out of range")
            if(max(cols) > ncol(.$mat) || min(cols) < 1) stop("column out of range")
            tmp <- proto(., expr = {
                set.vals <- function(., rs, cs, vals) {
                    if(max(rs) > length(.$rows) || min(rs) < 1) stop("row out of range")
                    if(max(cs) > length(.$cols) || min(cs) < 1) stop("column out of range")
                    .super$mat[.$rows[rs], .$cols[cs]] <- vals
                }
            })
            tmp$rows <- rows
            tmp$cols <- cols
            structure(tmp, class=c('my.sub.mat', 'proto', 'environment'))
        }
    }), class=c('my.mat', 'proto', 'environment'))
}

"[.my.mat" <- function(x, ...) get('mat', x)[...]
"[<-.my.sub.mat" <- "[<-.my.mat" <- function(x, i, j, value) {x$set.vals(i, j, value); as.proto(x)}
print.my.sub.mat <- function(x) print(get('mat',x)[get('rows', x), get('cols', x)])
print.my.mat <- function(x) print(get('mat',x))

a <- my.mat(matrix(1:(4*3),4,3))
b <- a$get.mat.ref(2:3, 2:3)
b[1,1] <- 42  # equivalent to b$set.vals(1,1,42)
b[1:2,2] <- 43
a[1,1] <- 44
a[rep(4, 2), 2:3] <- 45
b
a

#test passing sub
test <- function(x) x[2,1] <- 46
test(b)
a

dat <- a$mat
