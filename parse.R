ftext <- "test <- function(x, y) {
for(i in 1:10) {x + y + i}
}"

cat(paste(deparse(eval(parse(text=ftext)), control='all'), collapse='\n'))
paste(deparse(parse(text=ftext)), collapse='\n')

sink('d:/temp/fdump.txt')
cat(paste(deparse(eval(parse(text=deparse(lm, control='all'))), width.cutoff=256), collapse='\n'))
sink()

f <- paste(deparse(eval(parse(text=deparse(lm, control='all'))), width.cutoff=256), collapse='\n')
f <- deparse(eval(parse(text=deparse(lm, control='all'))), width.cutoff=256)
eval(parse(text=f))
