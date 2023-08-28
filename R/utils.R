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
    # U is a matrix of column-wise uniform values sampled from the space of X
    colmin <- apply(X, 2, min)
    colmax <- apply(X, 2, max)
    U <- matrix(0, ncol = ncol(X), nrow = m)
    for (i in 1:ncol(X)) {
      U[, i] <- runif(m, min = colmin[i], max = colmax[i])
    }
  } else {
    # The user has provided the uniform values in U.
  }

  # Random sample of m rows in X (without replacement)
  j <- sample(1:nrow(X), m)
  W <- X[j, , drop=FALSE]   # Need 'drop' in case X is single-column

  if(method=="simple") {
    # distance between each row of W and each row of X
    dwx <- as.matrix(pdist(W,X))
    # Caution: W[i,] is the same point as X[j[i],] and the distance between them is 0,
    # but we do not want to consider that when calculating the minimum distance
    # between W[i,] and X, so change the distance from 0 to Inf
    for(i in 1:m) dwx[i,j[i]] <- Inf
    # distance from each row of W to the NEAREST row of X
    dwx <- apply(dwx, 1, min)

    # distance between each row of U and each row of X
    dux <- as.matrix(pdist(U,X)) # rows of dux refer to U, cols refer to X
    # distance from each row of U to the NEAREST row of X
    dux <- apply(dux, 1, min)

    hop <- sum(dux^d) / sum( dux^d + dwx^d )
  }
  if(method=="torus") {
    rng <- t(apply(X,2,range))

    # Note: Since W is a sample from X, the 1st nearest point in X will
    # always be the same point with distance 0, so add 1 to k.
    nearw <- donut::nnt(X, W, k=k+1, torus=1:ncol(W), ranges=rng )
    dwx <- nearw$nn.dists[,k+1]

    # For U, find the 1st nearest point in X, k=1.
    nearu <- donut::nnt(X, U, k=k, torus=1:ncol(W), ranges=rng )
    dux <- nearu$nn.dists[,k]

    hop <- sum(dux^d) / sum( dux^d + dwx^d )
  }

  if(method=="boundedsphere" | method=="boundedcube"){
    # distance between each row of W and each row of X
    dwx <- as.matrix(pdist(W,X))
    for(i in 1:m) dwx[i,j[i]] <- Inf
    # distance from each row of W to the NEAREST row of X
    dwx <- apply(dwx, 1, min)

    # distance between each row of U and each row of X
    dux <- as.matrix(pdist(U,X)) # rows of dux refer to U, cols refer to X
    dux <- apply(dux, 1, min)

    if(method=="boundedsphere") const <- 1
    if(method=="boundedcube") const <- 2
    ukd <- (const*dux)^d
    wkd <- (const*dwx)^d
    N <- nrow(X)

    # I *think* this is the formula of Rotondi, page 560-561.
    # However, what happens if ukd=1? Then division by zero.
    # Also, this is supposed to have Beta(kM,kM) distribution, so it should
    # be between [0,1], but my example has values outside [0,1].
    # Should X be scaled to unit hypercube/hypersphere???
    hop <- (N-k+1) * sum( ukd/(1-ukd) ) /
      sum( (N-k+1)*ukd/(1-ukd) + (N-k)*(wkd/(1-wkd)) )
  }

  # You would think this would be faster, but it is not for our test cases:
  # stat = 1 / (1 + sum(dwx^d) / sum( dux^d ) )

  return( hop )
}




