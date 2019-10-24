#Packages needed:
install.packages("glmnet")
install.packages("rsample")
install_github("statsmaths/casl", force = TRUE)
install.packages("Rtools")
devtools::install_github("statsmaths/casl")

library(ggplot2)
library(devtools)
#devtools::install_github("statsmaths/casl", force=TRUE)

install.packages("MASS")
library(MASS)

library(roxygen2)
library(dplyr)
library(glmnet)
library(rsample)
library(crayon)
#library(casl)

# BUILD VIGNETTES WITH THIS FUNCTION:
devtools::build_vignettes()

usethis::use_testthat()
usethis::use_test('cross_validation')
usethis::use_test('ridge_regression')
