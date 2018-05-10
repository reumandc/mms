context("errcheck_n")
library(mms)

test_that("error checking capability", {
  comingfrom<-"fakefunname"
  dimmats<-5
  n<-3.4
  maxruns<-25
  expect_error(errcheck_n(comingfrom,dimmats,n,maxruns),paste0("Error in ",comingfrom,": n must be a whole number, maxruns must be a whole number or Inf"))

  n<-3
  maxruns<-5.2
  expect_error(errcheck_n(comingfrom,dimmats,n,maxruns),paste0("Error in ",comingfrom,": n must be a whole number, maxruns must be a whole number or Inf"))

  maxruns<-(-1)
  expect_error(errcheck_n(comingfrom,dimmats,n,maxruns),paste0("Error in ",comingfrom,": maxruns out of range"))
  
  n<-1
  maxruns<-10
  expect_error(errcheck_n(comingfrom,dimmats,n,maxruns),paste0("Error in ",comingfrom,": n must be at least 2 and not more than half the dimension of the matrices"))
  
  dimmats<-100
  n<-50
  maxruns<-Inf
  expect_error(errcheck_n(comingfrom,dimmats,n,maxruns),paste0("Error in ",comingfrom,": more LNOs than the max integer, try reducing n or using a lower maxruns"))
})









