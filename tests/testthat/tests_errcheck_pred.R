context("errcheck_pred")
library(mms)

test_that("test error handling abilities", {
  comingfrom<-"faketestfun"
  predlist<-list(c())
  lenmats<-5
  expect_error(errcheck_pred(comingfrom,predlist,lenmats),
               "Error in faketestfun: at least one predictor required")
  predlist<-list(c(1,3,6))
  expect_error(errcheck_pred(comingfrom,predlist,lenmats),
               "Error in faketestfun: predictor variable index out of bounds")
})


