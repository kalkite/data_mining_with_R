

dist_to_knn <- function(dataset,neighbors) {

  numrow <- dim(dataset)[1L]
  mxNN <- neighbors*2+2
  knndist <- matrix(0,nrow=mxNN,ncol=numrow)
  for (i in 1:numrow) {
    # find obervations that make up the k-distance neighborhood for observation dataset[i,]
    neighdist <- knneigh.vect(dataset[i,],dataset,neighbors)

    x <- length(neighdist)
    if (x > mxNN) {
      knndist <- rbind(knndist,matrix(rep(0,(x-mxNN)*numrow),ncol=numrow))
      mxNN <- x
    }
    knndist[1:x,i] <- neighdist
  }

  return(knndist[1:mxNN,])
}


k_nneigh_vect <- function(x,data,k) {

  temp <- as.matrix(data)
  numrow <- dim(data)[1L]
  dimnames(temp) <- NULL

  # subtract rowvector x from each row of data
  difference<- scale(temp, x, FALSE)

  # square and add all differences and then take the square root
  dtemp <- drop(difference^2 %*% rep(1, ncol(data)))
  dtemp <- sqrt(dtemp)

  # order the distances
  order_dist <- order(dtemp)
  nndist <- dtemp[order_dist]

  # find distance to k-nearest neighbor
  # uses k+1 since first distance in vector is a 0
  knndist <- nndist[k+1]

  # find neighborhood
  # eliminate first row of zeros from neighborhood
  neighborhood <- drop(nndist[nndist<=knndist])
  neighborhood <- neighborhood[-1]
  numneigh <- length(neighborhood)

  # find indexes of each neighbor in the neighborhood
  index.neigh <- order_dist[1:numneigh+1]

  # this will become the index of the distance to first neighbor
  num1 <- numneigh+3

  # this will become the index of the distance to last neighbor
  num2 <- numneigh+numneigh+2


  return(c(num1,num2,index.neigh,neighborhood))
}



reachability_density<- function(distdata,k) {

  p <- dim(distdata)[2]
  lrd <- rep(0,p)

  for (i in 1:p) {
    j <- seq(3,3+(distdata[2,i]-distdata[1,i]))
    # compare the k-distance from each observation to its kth neighbor
    # to the actual distance between each observation and its neighbors
    numneigh <- distdata[2,i]-distdata[1,i]+1
    temp <- rbind(diag(distdata[distdata[2,distdata[j,i]],distdata[j,i]]),distdata[j+numneigh,i])

    #calculate reachability
    reach <- 1/(sum(apply(temp,2,max))/numneigh)
    lrd[i] <- reach
  }
  lrd
}


local_outlier_factor <- function(data,k) {

  data <- as.matrix(data)

  # obtain the k nearest neighbors and their distance from each observation
  distdata <- dist.to.knn(data,k)
  p <- dim(distdata)[2L]

  # calculate the local reachability_densitydensity for each observation in data
  lrddata <- reachability(distdata,k)

  lof <- rep(0,p)

  # computer the local outlier factor of each observation in data
  for (i in 1:p) {
    nneigh <- distdata[2,i]-distdata[1,i]+1
    j <- seq(0,(nneigh-1))
    local.factor <- sum(lrddata[distdata[3+j,i]]/lrddata[i])/nneigh
    lof[i] <- local.factor
  }

  # return lof, a vector with the local outlier factor of each observation
  lof
}

