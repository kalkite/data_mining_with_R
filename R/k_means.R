random_centroids <- function(data, k) {
  # Check if the input is a data frame and convert to a matrix if needed
  if (is.data.frame(data)) {
    data <- as.matrix(data)
  }

  # Get the shape (number of rows and columns) of the matrix
  shape <- dim(data)
  num_sample <- shape[1]
  num_features <- shape[2]

  # Initialize a matrix to store the centroids
  centroids <- matrix(0, nrow = k, ncol = num_features)

  # Randomly select k data points as initial centroids
  random_indices <- sample(1:num_sample, k)
  centroids <- data[random_indices, , drop = FALSE]

  # Create a list with a named element for centroids
  result <- list(centroids = centroids)

  # Return the list
  return(result)
}

