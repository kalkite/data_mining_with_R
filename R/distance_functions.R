#' Euclidean Distance
#'  Calculate the Euclidean distance between two points in n-dimensional space.
#' @param point1 a numeric vector representing the first point.
#' @param point2 a numeric vector representing the second point.
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



#' Cosine Distance between Two Vectors
#'
#' Calculate the cosine distance between two vectors.
#'
#' @param point1 A numeric vector representing the first vector.
#' @param point2 A numeric vector representing the second vector.
#'
#' @return The cosine distance (a scalar value).
#' @export
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





