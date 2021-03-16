#' Linear Model Constructor
#'
#' This function does the linear regression depend on the input formula and data
#'   and return a table as result.
#'
#' @param formula Formula class object input shows the relationship between
#'   independ and depend variable.
#' @param data Data frame input contains the variables in the model.
#' @keywords inference, prediction
#'
#' @return A table includes \code{Estimate}, a column of Estimate Coefficients;
#'   \code{Std. Error}, a column of Residual Standard Error; \code{t value}, a
#'   column of \code{Estimate} divided by \code{Std.Error}; and \code{p value},
#'   a column of P-values corresponding to \code{t value} in the T distribution.
#'
#' @examples
#' data(mtcars)
#' my_lm(mpg ~ hp + wt, data = mtcars)
#'
#' @export
my_lm <- function(formula, data) {
  # Extract the x and y variables
  X <- model.matrix(formula, data)
  obs_frame <- model.frame(formula, data)
  Y <- model.response(obs_frame)

  # Calculate the "Estimate", "Std. Error", "t value", and "p value"
  df <- length(Y) - ncol(X)
  lm_coef <- solve(t(X) %*% X) %*% t(X) %*% Y
  est <- sum((Y - X %*% lm_coef)^2 / df)
  se <- sqrt(est * diag(solve(t(X) %*% X)))
  t_val <- (lm_coef - 0) / se
  p_val <- 2 * pt(abs(t_val), df, lower.tail = FALSE)

  # Put the results into a table
  summary_table <- cbind(lm_coef, se, t_val, p_val)
  colnames(summary_table) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  return(summary_table)
}
