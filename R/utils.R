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


<<<<<<< HEAD
=======
#' Calculate Stirling Numbers of the Second Kind
#'
#'A brute-force or exhaustive algorithm for finding a good clustering is to simply
#'generate all possible partitions of \eqn{n} points into \eqn{k} clusters, evaluate some optimization
#'score for each of them, and to retain the clustering that yields the best score. The
#'exact number of ways of partitioning \eqn{n} points into \eqn{k} non-empty and disjoint parts
#'is given by the Stirling numbers of the second kind,
#'
#'
#'\deqn{S(n,k) = \frac{1}{k!}\sum_{t=0}^{k} (-1)^t \binom{a}{b} (k-t)^n}
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























>>>>>>> f4209f3 (utils)

