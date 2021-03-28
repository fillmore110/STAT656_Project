#FROM: https://rpubs.com/esobolewska/dtw-time-series
#      Dynamic Time Warping (DTW) as a mean to cluster time series
#      Ewa Sobolewska


a <- c(AT, BE, BG, CH, CY, CA)
b <- runif(17, min=7, max=12)

emissions <- data.frame(name = c("AT", "BE", "BG", "CH", "CY", "CA","MX","EU","RS","UG")
                        , X2000=runif(10, min=7, max=12)
                        , X2001=runif(10, min=7, max=12)
                        , X2002=runif(10, min=7, max=12)
                        , X2003=runif(10, min=7, max=12)
                        , X2004=runif(10, min=7, max=12)
                        , X2005=runif(10, min=7, max=12)
                        , X2006=runif(10, min=7, max=12)
                        , X2007=runif(10, min=7, max=12)
                        , X2008=runif(10, min=7, max=12)
                        , X2009=runif(10, min=7, max=12)
                        , X2010=runif(10, min=7, max=12)
                        , X2011=runif(10, min=7, max=12)
                        , X2012=runif(10, min=7, max=12)
                        , X2013=runif(10, min=7, max=12)
                        , X2014=runif(10, min=7, max=12)
                        , X2015=runif(10, min=7, max=12)
                        , X2016=runif(10, min=7, max=12))

a1 <- c(7,9,6,9,12,6,4,6,8)
a2 <- c(5,6,4,3,9,5,6,8,9)

xrange <- range(1:9)
yrange <- range(c(a1,a2))

plot(xrange, yrange, type="n", xlab="time",
     ylab="value", xaxp  = c(0,10,10), yaxp  = c(3,12,9)) 
lines(a1, col='blue', type='l')
lines(a2, col='magenta', type='l')

dtw(a1,a2)$index1
dtw(a1,a2)$index2

plot(dtw(a1,a2), xlab="a1 - blue", ylab="a2 - magenta", xaxp  = c(0,10,10), yaxp = c(0,10,10))

plot(dtw(a1,a2, keep=TRUE), xlab="a1 - blue", ylab="a2 - magenta", xaxp  = c(0,10,10), yaxp = c(0,10,10), type="threeway")

plot(dtw(a1,a2, keep=TRUE), xaxp  = c(0,10,10), yaxp = c(0,10,10), type="twoway", col=c('blue', 'magenta'))


dtw(a1, a2)$stepPattern

dtw(a1, a2, step.pattern = symmetric1)$stepPattern


dtw_matrix = matrix(rep(c(0),81), nrow=9, ncol=9, byrow = TRUE)
dtw_matrix


dtw_matrix[1,1] = sqrt(a1[1]^2 + a2[1]^2)
dtw_matrix

for (i in 2:9){
  dtw_matrix[i,1] = sqrt((a1[i] - a2[1])^2) + dtw_matrix[i-1,1]
}
dtw_matrix

for (j in 2:9){
  dtw_matrix[1,j] = sqrt((a1[1] - a2[j])^2) + dtw_matrix[1,j-1]
}
dtw_matrix

for (i in 2:9){
  for (j in 2:9){
    dtw_matrix[i,j] = sqrt((a1[i] - a2[j])^2) + min(dtw_matrix[i,j-1], dtw_matrix[i-1,j], dtw_matrix[i-1,j-1])
  }
}
dtw_matrix


path = c(9,9) # starting with furthest place in matrix
i = 9
j = 9
while(i>1 & j>1){
  if (j == 1) {
    j = j - 1
  } else if (i == 1) {
    i = i - 1
  } else if (dtw_matrix[i-1,j] == min(dtw_matrix[i-1, j-1], dtw_matrix[i-1, j], dtw_matrix[i, j-1])){
    i = i - 1
  } else if (dtw_matrix[i-1,j-1] == min(dtw_matrix[i-1, j-1], dtw_matrix[i-1, j], dtw_matrix[i, j-1])){
    i = i - 1
    j = j - 1
  } else {
    j = j - 1
  }
  path = rbind(path, c(i,j))
}
path = rbind(path, c(1,1))


plot(dtw(a1,a2, step.pattern = symmetric1))
points(path[,1], path[,2], type = "l")


head(emissions)
require(BBmisc)
emissions.norm <- BBmisc::normalize(emissions[-1], method="standardize")
clust.pam1 <- tsclust(emissions.norm, type="partitional", k=2L:8L, distance="dtw", centroid="pam")

(x = sapply(clust.pam1, cvi))
index(x)
x[4,]
max(x[4,])
 plot(x[4,])
 which.max( x[1,] )

clust.pam <- tsclust(emissions.norm, type="partitional", k=4L, distance="dtw", clustering="pam")


clust.hier <- tsclust(emissions.norm, type = "h", k = 4L, distance = "dtw")
plot(clust.hier, type="sc")

cutree(clust.hier, k=6L)
cvi(clust.hier)
cvi(clust.pam)
