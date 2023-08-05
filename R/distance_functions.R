#' Euclidean Distance
#'  Calculate the Euclidean distance between two points in n-dimensional space.
#' @param point1 a numeric vector representing the first point.
#' @param point2 a numeric vector representing the second point.
#'
#' @details The Euclidean distance between two points
#' calculated as:\deqn{S(x,y) = \sqrt{\sum_{i=1}^{n} (x_i - y_i)^2}}
#'
#' @return The Euclidean distance (a scalar value).
#' @export
#'
#' @examples
#' euclidean_distance(c(1, 2, 3), c(4, 5, 6))
euclidean_distance <- function(point1, point2) {
  if (length(point1) != length(point2)) {
    stop("Both points should have the same number of dimensions.")
  }

  squared_diff <- (point1 - point2)^2
  distance <- sqrt(sum(squared_diff))
  return(distance)
}

#' Manhattan Distance
#'
#' Calculate the Manhattan distance between two points in n-dimensional space.
#'
#' @param point1  numeric vector representing the first point.
#' @param point2  numeric vector representing the second point.
#'
#' @details The Manhattan distance distance between two points
#' calculated as:\deqn{S(x,y) = \sum_{i=1}^{n} |x_i - y_i|}
#'
#' @return The Manhattan distance (a scalar value).
#' @export
#'
#' @examples
#' manhattan_distance(c(1, 2, 3), c(4, 5, 6))
manhattan_distance <- function(point1, point2) {
  if (length(point1) != length(point2)) {
    stop("Both points should have the same number of dimensions.")
  }

  abs_diff <- abs(point1 - point2)
  distance <- sum(abs_diff)
  return(distance)
}


#' Chebyshev Distance
#'
#' Calculate the Chebyshev distance between two points in n-dimensional space.
#'
#' @param point1 A numeric vector representing the first point.
#' @param point2 A numeric vector representing the second point.
#'
#'WWWWWW
#' @details The Chebyshev distance distance between two points
#' calculated as:\deqn{S(x,y) = max_i(|x_i - y_i|)}

#' @return The Chebyshev distance (a scalar value).
#' @export
#'
#' @examples
#' chebyshev_distance(c(1, 2, 3), c(4, 5, 6))
chebyshev_distance <- function(point1, point2) {
  if (length(point1) != length(point2)) {
    stop("Both points should have the same number of dimensions.")
  }

  abs_diff <- abs(point1 - point2)
  distance <- max(abs_diff)
  return(distance)
}



#' Mahalanobis Distance between Two Vectors (Pseudo-inverse approach)
#'
#' Calculate the Mahalanobis distance between two vectors using a pseudo-inverse approach.
#'
#' @param point1 A numeric vector representing the first vector.
#' @param point2 A numeric vector representing the second vector.
#'
#' @details The Mahalanobis distance distance between two points
#' calculated as:
#' \deqn{S(x,y) = \sqrt{\sum_{i=1}^{n} (x-y)^T C^{-1}(x-y)}}
#' where \eqn{C} is the Covariance matrix.
#' @return The Mahalanobis distance (a scalar value).
#' @export
#'
#' @examples
#' point1 <- c(1, 2, 3)
#' point2 <- c(4, 5, 6)
#' mahalanobis_distance(point1, point2)
mahalanobis_distance <- function(point1, point2) {
  if (!is.numeric(point1) || !is.numeric(point2) || length(point1) != length(point2)) {
    stop("Invalid input: point1 and point2 should be numeric vectors of the same length.")
  }

  # Combine the points to create a matrix
  combined_points <- rbind(point1, point2)

  # Calculate the covariance matrix
  cov_matrix <- cov(combined_points)

  # Use the pseudo-inverse (Moore-Penrose inverse) to handle singular covariance matrices
  inv_cov_matrix <- MASS::ginv(cov_matrix)

  # Center the points
  centered_point1 <- point1 - colMeans(combined_points)
  centered_point2 <- point2 - colMeans(combined_points)

  # Calculate the Mahalanobis distance
  mahalanobis_distance <- sqrt(t(centered_point1 - centered_point2) %*% inv_cov_matrix %*% (centered_point1 - centered_point2))

  return(mahalanobis_distance)
}


#' Canberra Distance between Two Vectors
#'
#' Calculate the Canberra distance between two vectors.
#'
#' @param point1 A numeric vector representing the first vector.
#' @param point2 A numeric vector representing the second vector.
#'
#' @details The Canberra distance distance between two points
#' calculated as:
#' \deqn{S(x,y) = \sum\limits_{i=1}^{n} \frac{|x_i - y_i|}{|x_i| + |y_i|} }
#'
#'
#' @return The Canberra distance (a scalar value).
#' @export
#'
#' @examples
#' point1 <- c(1, 2, 3)
#' point2 <- c(4, 5, 6)
#' canberra_distance(point1, point2)
canberra_distance <- function(point1, point2) {
  if (!is.numeric(point1) || !is.numeric(point2) || length(point1) != length(point2)) {
    stop("Invalid input: point1 and point2 should be numeric vectors of the same length.")
  }

  n <- length(point1)

  # Calculate the Canberra distance
  distance <- sum(abs(point1 - point2) / (abs(point1) + abs(point2)))

  return(distance)
}

# ************************ # Chebshev distance # ********************************
#' Cosine Distance between Two Vectors
#'
#' Calculate the cosine distance between two vectors.
#'
#' @param point1 A numeric vector representing the first vector.
#' @param point2 A numeric vector representing the second vector.
#'
#' @return The cosine distance (a scalar value).
#' @export
#' @details The Cosine similarity is the measure of the angle between two vectors
#' \deqn{S(x,y) =  \frac{x \cdot y}{\|x\| \|y\| }}
#'
#' Cosine distance calculated as
#' \deqn{S(x,y) =  1 - S_c(x,y) }
#'
#' @examples
#' point1 <- c(1, 2, 3)
#' point2 <- c(4, 5, 6)
#' cosine_distance(point1, point2)
cosine_distance <- function(point1, point2) {
  if (!is.numeric(point1) || !is.numeric(point2) || length(point1) != length(point2)) {
    stop("Invalid input: point1 and point2 should be numeric vectors of the same length.")
  }

  # Calculate the dot product of the two vectors
  dot_product <- sum(point1 * point2)

  # Calculate the magnitudes of the two vectors
  magnitude_point1 <- sqrt(sum(point1^2))
  magnitude_point2 <- sqrt(sum(point2^2))

  # Calculate the cosine distance
  distance <- 1 - (dot_product / (magnitude_point1 * magnitude_point2))

  return(distance)
}







#' Minkowski Distance
#'
#' This function calculates the Minkowski distance between two numeric vectors
#' \code{vector1} and \code{vector2}, using the parameter \code{p_value} to determine
#' the order of the distance.
#'
#' @param vector1 A numeric vector representing the first point.
#' @param vector2 A numeric vector representing the second point.
#' @param p_value A numeric value representing the order of the Minkowski distance.
#'
#' @return The Minkowski distance between \code{vector1} and \code{vector2} using the
#' specified \code{p_value}.
#'
#' @details calculated as
#' \deqn{S(x,y) =  (\sum\limits_{i=1}^{n} |x_i - y_i|^p)^{1/p}}
#'
#' @examples
#' # Example usage
#' vector1 <- c(1, 2, 3)
#' vector2 <- c(4, 5, 6)
#' p_value <- 2
#' distance <- minkowski_distance(vector1, vector2, p_value)
minkowski_distance <- function(vector1, vector2, p_value) {
  if (length(vector1) != length(vector2)) {
    stop("Input vectors must have the same length.")
  }

  distance <- sum(abs(vector1 - vector2)^p_value)^(1/p_value)

  return(distance)
}












