context("mmsclean")

test_that("test error catching functionality of mmsclean", {
  #test 1
  mats<-list(v1=matrix(1:16,4,4),v2=matrix(17:32,4,4),v3=matrix(33:48,4,4))
  resp<-4
  pred<-c(2,3)
  n<-2
  expect_error(mmsclean(mats,resp,pred,n),
               "Error in mmsclean: response variable index out of bounds",fixed=T)
  
  #test 2
  resp<-1
  pred<-2:4
  n<-2
  expect_error(mmsclean(mats,resp,pred,n),
               "Error in mmsclean: predictor variable index out of bounds",fixed=T)

  #test 3
  resp<-1
  pred<-1:3
  n<-2
  expect_error(mmsclean(mats,resp,pred,n),
               "Error in mmsclean: resp cannot also be in pred",fixed=T)

  #test 4
  resp<-1
  pred<-2:3
  n<-1
  expect_error(mmsclean(mats,resp,pred,n),
               "Error in mmsclean: n out of range",fixed=T)

  #test 5
  mats<-list(v1=matrix(1:16,4,4),v2=matrix(17:32,4,4),v3=matrix(5,5,5))
  resp<-1
  pred<-2:3
  n<-2
  expect_error(mmsclean(mats,resp,pred,n),
               "Error in mmsclean: all matrices must be same dimension and square",fixed=T)

})

test_that("test the output of mmsclean", {
  #test 1
  mats<-list(v1=matrix(1:16,4,4),v2=matrix(17:32,4,4),v3=matrix(33:48,4,4))
  resp<-2
  pred<-c(1,3)
  n<-2
  h<-mmsclean(mats,resp,pred,n)
  exp.ans<-list(mats=list(v2=matrix(c(NA,18,19,20,NA,NA,23,24,NA,NA,NA,28,NA,NA,NA,NA),4,4),
                          v1=matrix(c(NA,2,3,4,NA,NA,7,8,NA,NA,NA,12,NA,NA,NA,NA),4,4),
                          v3=matrix(c(NA,34,35,36,NA,NA,39,40,NA,NA,NA,44,NA,NA,NA,NA),4,4)),
                resp=1,pred=c(2,3))
  expect_equal(h,exp.ans)
})
