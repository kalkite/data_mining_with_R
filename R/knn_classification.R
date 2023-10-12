
library(tidyverse)
library(philentropy)
library(reshape)


my_knn <- function(train_data, train_labels, test=NA, k=7, metric="euclidean", normalize = TRUE, verbose = FALSE){
  # Normalizing if the user so says...
  if(normalize){
    train_data = as.data.frame(lapply(train_data, scale, center = TRUE, scale = TRUE))
  }


  # Checking if we have test data to use or not
  if(is.na(test)){
    test = train_data
  }

  # Declaring our output vector
  prediction = vector("logical", length(test[,1]))
  # For every datapoint we want to know the category of:
  for(i in 1:nrow(test)){
    if(verbose)
      print(paste("Going for test sample ", i))

    # We extract said datapoint
    sample = test[i,]

    # We create a two column, long dataframe that contains on one column
    # every point in train_data, and on the other column the current
    # sample we are processing repeated as many times as the # of train
    # data samples.
    rbinds = apply(train_data, MARGIN = 1, rbind, sample)

    # That step allows us to apply the distance function
    # using lapply, which is orders of magnitude faster.
    distances = lapply(rbinds, distance, metric)

    # Having the distances vector, we join it with our labels
    # to create a little helper dataframe and set its colnames.
    results = data.frame(as.vector(distances, "numeric"), as.vector(train_labels))
    colnames(results) = c("Distances", "Labels")

    # We may now sort the results by distance and take only
    # the k first.
    results = results[order(results$Distances),]
    k_results = results[1:k,]

    # Having this little (k, 2) dataframe we can count
    # how much each label appears, take the maximum and
    # put it in the predictions array, to continue with the
    # next sample in the test array.
    count = k_results %>% count(Labels)
    predicted_category = count$Labels[which.max(count$n)]

    prediction[i] = predicted_category
  }
  return(prediction)
}

eval_knn <- function(train_data, train_labels, test=NA, k_neighbors=seq(5, 9, by=2), metrics = c("euclidean", "manhattan"), verbose = 1){
  methods = vector("character", length = (length(metrics) * length(k_neighbors)))
  k_values = vector("numeric", length = (length(metrics) * length(k_neighbors)))
  accuracies = vector("numeric", length = (length(metrics) * length(k_neighbors)))

  i = 1
  for(m in metrics){
    if(verbose >= 1){
      print(paste("Going for method", m))
    }

    for(k in k_neighbors){
      if(verbose == 2){
        print(paste("Going for k", k))
      }

      prediction = my_knn(train_data, train_labels, test=test, k=k, metric=m)
      pred_diag = data.frame(prediction, train_labels)
      colnames(pred_diag) = c("prediction", "labels")

      accuracy = (pred_diag %>% filter(prediction == labels) %>% count()) / dim(pred_diag[,0])

      accuracies[i] = accuracy
      k_values[i] = k
      methods[i] = m
      i = i + 1
    }
  }

  results = data.frame(cbind(methods, k_values, accuracies))
  results = as.data.frame(lapply(results, unlist))
  colnames(results) = c("Method", "K", "Accuracy")


  print(
    ggplot(results, aes(x = K, y = Accuracy, group = Method)) +
      geom_line(aes(color = Method)) +
      geom_point(aes(color = Method))
  )

  return(results)
}
