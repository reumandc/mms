context("errcheck_mats")
library(mms)

test_that("test error handling abilities", {
  comingfrom<-"faketestfun"
  mats<-list(m1=matrix("word"),10,10,m2=matrix(5,10,10),m3=matrix(6,10,10),m4=matrix(7,10,10))
  expect_error(errcheck_mats(comingfrom,mats),paste0("Error in ",comingfrom,": all matrices must be numeric"))

  mats<-list(matrix(1:16,4,4),matrix(17:32,4,4))
  expect_error(errcheck_mats(comingfrom,mats),paste0("Error in ",comingfrom,": all matrices must be symmetric"))

  mats<-list(m1=matrix(4,4,4),m2=matrix(5,5,5))
  expect_error(errcheck_mats(comingfrom,mats),paste0("Error in ",comingfrom,": all matrices must be same dimension and square"))
  mats<-list(m1=matrix(1,4,3),m2=matrix(2,4,4))
  expect_error(errcheck_mats(comingfrom,mats),paste0("Error in ",comingfrom,": all matrices must be same dimension and square"))

  mats<-list(matrix(4,4,4),matrix(5,4,4))
  mats[[1]][1,2]<-NA
  mats[[1]][2,1]<-NA
  expect_error(errcheck_mats(comingfrom,mats),paste0("Error in ",comingfrom,": non-finite off diagonal entries not allowed"))
})