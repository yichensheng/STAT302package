#' Random Forest Cross Validation
#'
#' This function does the Cross Validation test for the random forest predict
#'   model. This function is specifically using \code{bill_length_mm},
#'   \code{bill_depth_mm}, and \code{flipper_length_mm} to predict
#'   \code{body_mass_g} from the data \code{my_penguins}.
#'
#' @param k Numeric input of number of folds.
#' @keywords prediction
#'
#' @return a numeric of the cross-validation error value.
#'
#' @examples
#' my_rf_cv(5)
#'
#' @export
my_rf_cv <- function(k) {
  # Remove NA from penguins data
  penguins_data <- na.omit(MyStat302Package::my_penguins)
  # Get the X variable data
  X_data <- penguins_data[, 3:5]
  # Get the Y variable data
  Y_data <- penguins_data[, 6]
  # Split data in 5 parts, randomly
  fold <- k
  inds <- sample(rep(1:fold, length = nrow(X_data)))
  # Construct the data frame with x, y and group index
  data <- data.frame("x" = X_data, "y" = Y_data, "split" = inds)

  # k folds Cross Validation test
  err_vec <- c()
  for (i in 1:fold) {
    # Construct the training data of x
    data_train <- data %>% filter(split != i)
    # Construct the testing data of x
    data_test <- data %>% filter(split == i)
    # Train our models
    model <- randomForest(body_mass_g ~
                            x.bill_length_mm + x.bill_depth_mm + x.flipper_length_mm,
                          data = data_train, ntree = 100)
    # Predict the y-hat
    pred_value <- predict(model, data_test[, 1:3])
    # Calculate the MSE
    err_vec[i] <- mean((pred_value - data_test[, 4])^2)
  }

  # Calculate the average MSE and return it
  ave_MSE <- mean(err_vec)
  return(ave_MSE)
}
