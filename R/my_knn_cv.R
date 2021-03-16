#' K-Nearest Neighbors Cross-Validation
#'
#' This function does the Cross Validation test for the kth nearest neighbor
#'   predict model.
#'
#' @param train Data frame input that record X variable.
#' @param cl Vector input of true class value of training data.
#' @param k_nn, Numeric input representing the number of neighbors.
#' @param k_cv, Numeric input representing the number of folds.
#' @keywords prediction
#'
#' @return a list includes \code{class}, a vector of the predicted class y-hat
#'   for all observations, \code{cv_err}, a numeric of the cross-validation
#'   misclassification error.
#'
#' @examples
#' penguins_data <- na.omit(my_penguins)
#' train <- penguins_data[, 3:6]
#' cl <- penguins_data$species
#' my_knn_cv(train, cl, 5, 5)
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  # Split data in k_cv parts, randomly
  fold <- k_cv
  inds <- sample(rep(1:fold, length = nrow(train)))
  # Construct the data frame with x, y and group index
  data <- data.frame("x" = train, "y" = cl, "split" = inds)

  # k_cv folds Cross Validation test
  n = ncol(data) - 2
  predict_list <- list()
  rate_vec <- vector()
  for (i in 1:fold) {
    # Construct the training data of x, remove the y and split column
    data_train <- data %>% dplyr::filter(split != i)
    data_train <- data_train[, 1:n]
    # Construct the testing data of x, remove the y and split column
    data_test <- data %>% dplyr::filter(split == i)
    data_test <- data_test[, 1:n]
    # Construct the factor of training data's y
    f1 <- factor(as.vector(cl[inds != i]))
    # Train our models and predict the testing data
    predict_list[[i]] <- knn(data_train, data_test, f1, k = k_nn)
    # Calculate the misclassification rate
    rate_vec[i] <- sum(predict_list[[i]] != cl[inds == i]) / length(cl[inds == i])
  }

  # Use the full data to train the model
  f1 <- factor(as.vector(cl))
  class <- knn(data[, 1:n], data[, 1:n], cl, k_nn)
  # Calculate the CV error
  cv_error <- mean(rate_vec)
  # Return the result list
  return(list(class, cv_error))
}
