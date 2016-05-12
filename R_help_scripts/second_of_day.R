toDaySecond <- function(date.string, format.string='%d%b%Y:%T', tz='') {
    d <- as.POSIXct(date.string, format=format.string, tz=tz)
    sum(mapply(function(f, l) as.numeric(format(d, format=f)) * l, c('%H', '%M', '%S'), c(3600, 60, 1)))
}

toDaySecond('04MAY2011:08:19:00')
    
