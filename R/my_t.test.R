#' T-test Calculator
#'
#' This function does the T-test with the input value, return the result as a list.
#'
#' @param x Numeric vector input, containing the values we are going to test.
#' @param alternative String input specifying alternative hypothesis, defaults
#'   to \code{"two.sided"}.
#' @param mu Numeric input, indicating the null hypothesis value of the mean,
#'   defaults to \code{0}.
#' @keywords inference
#'
#' @return A list includes \code{test_stat}, the numeric test statistic;
#'   \code{df}, the degrees of freedom; \code{alternative}, the value of the
#'   parameter alternative; and \code{p_val}, the numeric p-value.
#'
#' @importFrom stats model.frame model.matrix model.response na.omit predict pt sd
#' @importFrom dplyr filter
#' @importFrom class knn
#' @importFrom randomForest randomForest
#'
#' @examples
#' x <- rnorm(10, mean = 0, sd = 1)
#' my_t.test(x)
#' my_t.test(x, alternative = "less")
#' my_t.test(x, alternative = "greater", mu = 1)
#'
#' @export
my_t.test <- function(x, alternative = "two.sided", mu = 0) {
  # Calculate the estimate mean, df, standard error, and t value
  est <- mean(x)
  df <- length(x) - 1
  se <- sd(x) / sqrt(df + 1)
  t_obs <- (est - mu) / se

  # Calculate the P-value depends on the alternative parameter
  if (alternative == "less") {
    p_value <- pt(t_obs, df)
  } else if (alternative == "greater") {
    p_value <- pt(t_obs, df, lower.tail = FALSE)
  } else {
    p_value <- 2 * pt(abs(t_obs), df, lower.tail = FALSE)
  }

  # Put the result into a list form
  result <- list("test_stat" = t_obs,
                 "df" = df,
                 "alternative" = alternative,
                 "p_val" = p_value)
  return(result)
}
