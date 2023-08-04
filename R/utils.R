#' Entropy
#'
#' @param probs A numeric vector representing the probabilities of different outcomes. The probabilities should sum up to 1.
#'
#' @return The entropy value calculated from the input probability distribution.
#' @details The entropy of a random variable X with probability distribution P is
#'  calculated as: \deqn{E(v_i)= -\sum_{i} p_i \cdot \log_2(p_j)}{-sum(p(x_i) * log2(p(x_i)))},
#' where \eqn{p(x_i)}{p(x_i)} is the probability of outcome \eqn{x_i}{x_i}.
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

  # Remove zero probabilities to avoid issues with log(0)
  probs <- probs[probs > 0]

  # Calculate entropy
  entropy_value <- -sum(probs * log2(probs))

  return(entropy_value)
}
