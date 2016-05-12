#-----------------------------------------------------------------------
# get the distance between two points on the globe.
#
# args:
# lat1 - latitude of first point.
# long1 - longitude of first point.
# lat2 - latitude of first point.
# long2 - longitude of first point.
#-----------------------------------------------------------------------
greatCircleDistance <- function(lat1, long1, lat2, long2, radius=6372.795){
    sf <- pi/180
    lat1 <- lat1*sf
    lat2 <- lat2*sf
    long1 <- long1*sf
    long2 <- long2*sf
    lod <- abs(long1-long2)
    radius * atan2(
        sqrt((cos(lat1)*sin(lod))**2 + (cos(lat2)*sin(lat1)-sin(lat2)*cos(lat1)*cos(lod))**2),
        sin(lat2)*sin(lat1)+cos(lat2)*cos(lat1)*cos(lod)
    )
}

#-----------------------------------------------------------------------
# Calculate the nearest point using latitude and longitude.
# and attach the other args and nearest distance from the
# other data.frame.
#
# args:
# x as you describe 'track'
# y as you describe 'classif'
# xlongnme name of longitude variable in x
# xlatnme name of latitude location variable in x
# ylongnme name of longitude location variable on y
# ylatnme name of latitude location variable on y
#-----------------------------------------------------------------------
dist.merge <- function(x, y, xlongnme, xlatnme, ylongnme, ylatnme){
    tmp <- t(apply(x[,c(xlongnme, xlatnme)], 1, function(x, y){
        dists <- apply(y, 1, function(x, y) greatCircleDistance(x[2], x[1], y[2], y[1]), x) #sqrt((x[1]-y[1])^2 + (x[2]-y[2])^2), x)
        cbind(1:nrow(y), dists)[dists == min(dists),,drop=F][1,]
    }
    , y[,c(ylongnme, ylatnme)]))
    tmp <- cbind(x, min.dist=tmp[,2], y[tmp[,1],-match(c(ylongnme, ylatnme), names(y))])
    row.names(tmp) <- NULL
    tmp
}

# demo
track <- data.frame(xt=runif(10,0,360), yt=rnorm(10,-90, 90))
classif <- data.frame(xc=runif(10,0,360), yc=rnorm(10,-90, 90), v1=letters[1:20], v2=1:20)

dist.merge(track, classif, 'xt', 'yt', 'xc', 'yc')
        
