#' Cross-Validation for Ridge Regression
#'
#' @description finding optimal lambdas with Ridge Regression
#' @description sources: class sessions, casl book, will
#'
#' @param form form "y~..."
#' @param data a data frame
#' @param folds the number of folds used for cross-validation
#' @param lambdas a vector of lambdas
#' @param contrasts
#'
#' @return a tibble containing a summary of the statistics
#' @import dplyr ggplot2


cross_validation <- function(formula, data, folds= 5,
                             lambdas= seq(0, 0.5, 0.01),
                             contrasts= NULL){
  i <- lambda <- `.` <- lower <- upper <- NULL

  folds <- vfold_cv(data,folds)

  rmses<- foreach(lambda = lambdas, .combine = rbind) %dopar% {
    foreach(i = seq_len(nrow(folds)), .combine = c) %do% {
      casl_util_rmse(
        testing(folds$splits[[i]])[[as.character(formula[2])]],
        predict(ridge_regression(formula, training(folds$splits[[i]]),
                                 lambda = lambda, contrasts= contrasts),
                testing(folds$splits[[i]])))
    }
  }

  results <- tibble(mean =apply(rmse, 1, mean),
                    sd = apply(rmse, 1, sd),
                    lambda = lambdas) %>%
    mutate(upper = mean + 2 * sd / nrow(.),
           lower = mean - 2 * sd / nrow(.))

  lambda_min <- results$lambda[which.min(results$mean)]

}

devtools::document()
