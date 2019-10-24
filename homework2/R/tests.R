# TESTS

### RIDGE REGRESSION TESTING
library(testthat)


context("Test the output of ridge_regression().")

test_that("Your ridge_regression() function works in an easy case.", {

  data(iris)

  fit_lm_ridge <- lm.ridge(Sepal.Length ~ ., iris, lambda = 0.1)

  fit_ridge_regression <- ridge_regression(Sepal.Length  ~ ., iris, lambda = 0.1)

  expect_equivalent(coef(fit_lm_ridge), fit_ridge_regression,
                    tolerance = 1e-3)
})

test_that("Your linear_model() function works with contrasts.", {

  data(iris)

  fit_lm_ridge <- lm.ridge(Sepal.Length ~ ., iris,
                           contrasts = list(Species = "contr.sum"))

  fit_ridge_regression <- ridge_regression(Sepal.Length  ~ ., iris, contrasts = list(Species = "contr.sum"))

  expect_equivalent(coef(fit_lm_ridge), fit_ridge_regression,
                    tolerance = 1e-3)
})


### RIDGE REGRESSION CROSS-VALIDATION TEST


library(testthat)

context("Test the output of cv_ridge_regression().")

test_that("Your cv_ridge_regression() function finds the same lambda.min with the same inputs in an easy case.", {

  skip("cv_ridge_regression is currently unstable.")

  data(iris)

  fit_cv_glmnet <- cv.glmnet(model.matrix(Sepal.Length ~ ., iris), as.matrix(iris[,1]), alpha = 0, lambda = seq(0, 1, 0.03))

  fit_cv_ridge_regression <- cross_validation(Sepal.Length  ~ ., iris, folds = 2)

  expect_equivalent(fit_cv_glmnet$lambda.min, fit_cv_ridge_regression$lambda_min,
                    tolerance = 1e-3)
})

test_that("Your cv_ridge_regression() function works with contrasts.", {

  skip("cv_ridge_regression is currently unstable.")

  data(iris)

  fit_cv_glmnet <- cv.glmnet(model.matrix(Sepal.Length ~ ., iris,
                                          contrasts.arg = list(Species = "contr.sum")),
                             as.matrix(iris[,1]), alpha = 0, lambda = seq(0, 1, 0.03))

  fit_cv_ridge_regression <- cv_ridge_regression(Sepal.Length  ~ ., iris, contrasts = list(Species = "contr.sum"), folds = 2)

  expect_equivalent(fit_cv_glmnet$lambda.min, fit_cv_ridge_regression$lambda_min,
                    tolerance = 1e-3)
})


