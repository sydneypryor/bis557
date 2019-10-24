# TESTS

### RIDGE REGRESSION TESTING
library(testthat)


# MASS package corrupted, copied lm.ridge function code from https://github.com/cran/VR/blob/master/MASS/R/lm.ridge.R
lm.ridge <- function(formula, data, subset, na.action,
                     lambda = 0, model = FALSE, x = FALSE, y = FALSE, contrasts = NULL, ...)
{
  m <- match.call(expand.dots = FALSE)
  m$model <- m$x <- m$y <- m$contrasts <- m$... <- m$lambda <- NULL
  m[[1L]] <- as.name("model.frame")
  m <- eval.parent(m)
  Terms <- attr(m, "terms")
  Y <- model.response(m)
  X <- model.matrix(Terms, m, contrasts)
  n <- nrow(X); p <- ncol(X)
  offset <- model.offset(m)
  if(!is.null(offset)) Y <- Y - offset
  if(Inter <- attr(Terms, "intercept"))
  {
    Xm <- colMeans(X[, -Inter])
    Ym <- mean(Y)
    p <- p - 1
    X <- X[, -Inter] - rep(Xm, rep(n, p))
    Y <- Y - Ym
  } else Ym <- Xm <- NA
  Xscale <- drop(rep(1/n, n) %*% X^2)^0.5
  X <- X/rep(Xscale, rep(n, p))
  Xs <- svd(X)
  rhs <- t(Xs$u) %*% Y
  d <- Xs$d
  lscoef <-  Xs$v %*% (rhs/d)
  lsfit <- X %*% lscoef
  resid <- Y - lsfit
  s2 <- sum(resid^2)/(n - p - Inter)
  HKB <- (p-2)*s2/sum(lscoef^2)
  LW <- (p-2)*s2*n/sum(lsfit^2)
  k <- length(lambda)
  dx <- length(d)
  div <- d^2 + rep(lambda, rep(dx,k))
  a <- drop(d*rhs)/div
  dim(a) <- c(dx, k)
  coef <- Xs$v %*% a
  dimnames(coef) <- list(names(Xscale), format(lambda))
  GCV <- colSums((Y - X %*% coef)^2)/(n-colSums(matrix(d^2/div, dx)))^2
  res <- list(coef = drop(coef), scales = Xscale,
              Inter = Inter, lambda = lambda, ym = Ym, xm = Xm,
              GCV = GCV, kHKB = HKB, kLW = LW)
  class(res) <- "ridgelm"
  res
}


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


