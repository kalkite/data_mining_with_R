#' Entropy
#'
#' @param probs A numeric vector representing the probabilities of different outcomes. The probabilities should sum up to 1.
#'
#' @return The entropy value calculated from the input probability distribution.
#' @details The entropy of a random variable X with probability distribution P is
#' calculated as: \deqn{-\sum_{i} p(x_i) \cdot \log_2(p(x_i))}{-sum(p(x_i) * log2(p(x_i)))},
#' where \eqn{p(x_i)}{p(x_i)} is the probability of outcome \eqn{x_i}{x_i}.
#'
#' @export
#'
#' @examples
#' # Example usage
#' probabilities <- c(0.2, 0.3, 0.1, 0.4)
#' entropy_value <- entropy(probabilities)
#' print(paste("Entropy:", entropy_value))
entropy <- function(probs) {
  # Make sure the probabilities sum up to 1
  if (abs(sum(probs) - 1) > 1e-10) {
    stop("Probabilities must sum up to 1.")
  }


  probs <- probs[probs > 0]

  entropy_value <- -sum(probs * log2(probs))

  return(entropy_value)
}



#' Calculate Stirling Numbers of the Second Kind
#'
#'A brute-force or exhaustive algorithm for finding a good clustering is to simply
#'generate all possible partitions of \eqn{n} points into \eqn{k} clusters, evaluate some optimization
#'score for each of them, and to retain the clustering that yields the best score. The
#'exact number of ways of partitioning \eqn{n} points into \eqn{k} non-empty and disjoint parts
#'is given by the Stirling numbers of the second kind,
#'
#'
#'\deqn{S(n,k) = \frac{1}{k!}\sum_{t=0}^{k} (-1)^t \binom{k}{t} (k-t)^n}
#'
#' Each point can be assigned to any one of the \eqn{k} clusters, so there are at
#' most \eqn{k^n} possible clusterings. However, any permutation of the \eqn{n} clusters within a
#' given clustering yields an equivalent clustering, therefore, there are \eqn{O(k^n/k!)}  clusterings of n points into \eqn{k} groups
#'
#' @param n The total number of objects.
#' @param k The number of non-empty subsets.
#' @return The Stirling number of the second kind \eqn{S(n, k)}.
#' @export
#'
#' @examples
#' stirling_second(5, 3)
#'
#' @references
#' Wikipedia contributors. (2021, June 12). Stirling numbers of the second kind. In Wikipedia, The Free Encyclopedia.
#' Retrieved from \url{https://en.wikipedia.org/wiki/Stirling_numbers_of_the_second_kind}
#'
stirling_second <- function(n, k) {
  if (n == 0 & k == 0) {
    return(1)
  } else if (n > 0 & k == 0) {
    return(0)
  } else if (n == 0 & k > 0) {
    return(0)
  } else {
    return(k * stirling_second(n - 1, k) + stirling_second(n - 1, k - 1))
  }
}





library(pdist)
library(donut)
library(RANN)
#' Hopkins Statistics
#'
#' \deqn{H = \frac{\sum_{i=1}^{r} \beta_i}{\sum_{i=1}^{r}(\alpha_i+\beta_i) }}
#'
#' @return Data (matrix or data.frame) to check clusterability.
#' @export
#'
#'
#' @examples
#' set.seed(1)
#' hopkins(iris[, 1:4], m=15) # .9952293
#'
#' hop <- rep(NA, 100)
#' for(i in 1:100){
#'   hop[i] <- hopkins(iris[,1:4], m=8)
#' }
#' mean(hop)
hopkins_stat <- function (X, m=ceiling(nrow(X)/10), d=ncol(X), k=1, U=NULL, method="simple") {

  if (!(is.matrix(X)) & !(is.data.frame(X)))
    stop("X must be data.frame or matrix")

  if (m >= nrow(X))
    stop("m must be no larger than num of samples")

  if(missing(U)) {
    colmin <- apply(X, 2, min)
    colmax <- apply(X, 2, max)
    U <- matrix(0, ncol = ncol(X), nrow = m)
    for (i in 1:ncol(X)) {
      U[, i] <- runif(m, min = colmin[i], max = colmax[i])
    }
  } else {

  }

  # Random sample of m rows in X (without replacement)
  j <- sample(1:nrow(X), m)
  W <- X[j, , drop=FALSE]   # Need 'drop' in case X is single-column

  if(method=="simple") {
    dwx <- as.matrix(pdist(W,X))

    for(i in 1:m) dwx[i,j[i]] <- Inf
    dwx <- apply(dwx, 1, min)

    dux <- as.matrix(pdist(U,X)) # rows of dux refer to U, cols refer to X

    dux <- apply(dux, 1, min)

    hop <- sum(dux^d) / sum( dux^d + dwx^d )
  }
  if(method=="torus") {
    rng <- t(apply(X,2,range))

    nearw <- donut::nnt(X, W, k=k+1, torus=1:ncol(W), ranges=rng )
    dwx <- nearw$nn.dists[,k+1]

    # For U, find the 1st nearest point in X, k=1.
    nearu <- donut::nnt(X, U, k=k, torus=1:ncol(W), ranges=rng )
    dux <- nearu$nn.dists[,k]

    hop <- sum(dux^d) / sum( dux^d + dwx^d )
  }

  if(method=="boundedsphere" | method=="boundedcube"){
    dwx <- as.matrix(pdist(W,X))
    for(i in 1:m) dwx[i,j[i]] <- Inf
    dwx <- apply(dwx, 1, min)

    dux <- as.matrix(pdist(U,X)) #
    dux <- apply(dux, 1, min)

    if(method=="boundedsphere") const <- 1
    if(method=="boundedcube") const <- 2
    ukd <- (const*dux)^d
    wkd <- (const*dwx)^d
    N <- nrow(X)

    hop <- (N-k+1) * sum( ukd/(1-ukd) ) /
      sum( (N-k+1)*ukd/(1-ukd) + (N-k)*(wkd/(1-wkd)) )
  }
  return( hop )
}



hopkins_stat_pval <- function(x,n) {
  if(x > 0.5)
    1 - (pbeta(x, n, n) - pbeta(1-x, n, n) )
  else
    1 - (pbeta(1-x, n, n) - pbeta(x, n, n) )
}

if(0){
  D <- 8 # dimension of data, columns(X)
  N <- 5000 # number of events, rows(X)
  M <- 8 # number of events sampled
  B <- 1000 # number of simulations

  #scale01 <- function(x){ (x-min(x))/(max(x)-min(x)) }
  set.seed(12345)
  hop1 <- hop2 <- NULL
  for(ii in 1:B){
    X <- matrix(runif(N*D, min=-1, max=1), ncol=D)
    # calculate radial distance from origin for each point
    rad <- apply(X, 1, function(x) sqrt(sum((x-0)^2)))
    X <- X[rad < 1.0,]

    # We need to scale the data into unit hypercube
    # X <- apply(X, 2, scale01)
    # Since this is a simulation study, we can use the first M rows
    # as random origins in the unit hypersphere
    hop1[ii] <- hopkins_stat(X[-c(1:M),], U=X[1:M,], m=M, d=D)
    hop2[ii] <- hopkins_stat(X[-c(1:M),], U=X[1:M,], m=M, d=D, method="boundedsphere")
  }

  # Now the plots
  plot(density(hop1), col="red", xlim=c(0,1), main="", xlab="", ylim=c(0,4))
  lines(density(hop2), col="blue")
  xv <- seq(0,1,length=100)
  lines(xv, dbeta( xv, M, M) , col="black", lwd=2)
  legend("topleft",
         c("Hopkins", "Modified Hopkins", "Beta(M,M)"),
         text.col=c("red","blue","black")
  )
}



