
#source("generic/lib-generic.R")

rm(list = ls())
source("https://raw.githubusercontent.com/mobilegenome/lib-common/master/generic/lib-generic.R")

a_complex_test_function <- function() {
  x <- 3
  y <- 4
  z <- x*y
  return(z)
}

test <- conditional_RDS(test-object, "/tmp/test-r.object.Rds", a_complex_test_function)
