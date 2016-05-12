dat <- data.frame(col_a=c(11,13), col_b=c(12,14), col_c=c("10", "20|30|40"))
do.call(rbind, mapply(function(X, Y) {
        y.bits <- as.numeric(unlist(strsplit(Y, '|', fixed=T)))
        cbind(matrix(rep(X, each=length(y.bits)), ncol=2), y.bits)
    },
    as.data.frame(t(dat[,1:2])), dat[,3], SIMPLIFY=F)
)
