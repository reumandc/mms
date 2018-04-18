context("mmsvarwts")

test_that("tests for mmsvarwts", {
  #test 1
  mats<-list(resp=matrix(1:9,3,3),pred1=matrix(10:18,3,3),pred2=matrix(19:27,3,3))
  resp<-1
  pred<-c(2,3)
  n<-1
  varnames<-c("response","predictor1")
  expect_error(mmsvarwts(pred, mmsmodwts(mats, nrand=5, n, maxruns=5), varnames),
               "Error in mmsvarwts: varnames must be the same length as pred", fixed=T)
  
})