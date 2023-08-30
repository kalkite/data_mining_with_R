# Define the Fisher's Score function
#' Compute Fisher Score for Features in filter methods.
#'
#' This function calculates the Fisher Score for each feature in a dataset
#' based on the provided class labels. The Fisher Score is a measure of
#' the significance of a feature in discriminating between different classes.
#'
#' @param data A data frame containing the independent variables (features).
#' @param labels A vector of class labels corresponding to the samples in the data frame.
#'
#' @details
#' The Fisher Score (F_s) is computed using the formula:
#' \deqn{F_s = \frac{\sum_{j=1}^{K} p_j(\mu_{j}^{i}-\mu^i)^2}{\sum_{j=1}^{K}p_j (\sigma_{j}^i)^{2}}}
#'
#'where \eqn{\mu_{j}^{i}} and \eqn{\sigma_{j}^i} are the mean and standard deviation of the \eqn{j^-th} -class and \eqn{i^th} feature,
#'\eqn{p_j}  is the proportion  of data points of class belonging to the class \eqn{j}.
#'Greater Fisher's score values indicate greater discriminating power of the feature.
#'
#' @return A data frame containing Fisher Scores for each feature.
#' @export
#'
#' @examples
#' data(iris)
#' iris_data <- iris[, 1:4]
#' iris_labels <- iris$Species
#' fisher_score_df <- fishers_score(iris_data, iris_labels)
#' print(fisher_score_df)
fishers_score <- function(data, labels) {
  cat(rep('==', 40), '\n')

  data_length <- nrow(data)
  list_of_classes <- unique(labels)
  number_of_classes <- length(list_of_classes)
  cat(paste("Data contains:", number_of_classes, "classes.\n"))

  fishers_score_frame <- data.frame(matrix(NA, nrow = 1, ncol = ncol(data)))
  colnames(fishers_score_frame) <- colnames(data)

  for (column in colnames(data)) {
    column_mean <- mean(data[[column]])
    numerator <- 0
    denominator <- 0

    for (label in list_of_classes) {
      indexes <- (labels == label)
      class_in_data <- data[indexes, column]
      class_mean <- mean(class_in_data)
      class_std <- sd(class_in_data)
      class_proportion <- sum(indexes) / data_length
      numerator <- numerator + class_proportion * (class_mean - column_mean)^2
      denominator <- denominator + class_proportion * class_std^2
    }

    if (denominator != 0) {
      fishers_score_frame[1, column] <- numerator / denominator
    } else {
      fishers_score_frame[1, column] <- 0
    }
  }

  cat("Fisher's score(s) has/have been computed.\n")
  fdf <- fishers_score_frame[1, !is.na(fishers_score_frame[1, ])]
  fisher_score_df <- as.data.frame(t(fdf))
  cat(rep('==', 40), '\n')
  return(fisher_score_df)
}
