context("mmsclean")

test_that("test error catching functionality of mmsclean", {
  #test 1
  mats<-list(resp=matrix(1:9,3,3),pred1=matrix(10:18,3,3),pred2=matrix(19:27,3,3))
  resp<-4
  pred<-c(2,3)
  n<-1
  expect_error(mmsclean(mats,resp,pred,n),
               "Error in mmsclean: response variable index out of bounds",fixed=T)
  
  #test 2
  mats<-list(resp=matrix(1:9,3,3),pred1=matrix(10:18,3,3),pred2=matrix(19:27,3,3))
  resp<-1
  pred<-2:4
  n<-1
  expect_error(mmsclean(mats,resp,pred,n),
               "Error in mmsclean: predictor variable index out of bounds",fixed=T)

  #test 3
  mats<-list(resp=matrix(1:9,3,3),pred1=matrix(10:18,3,3),pred2=matrix(19:27,3,3))
  resp<-1
  pred<-1:3
  n<-1
  expect_error(mmsclean(mats,resp,pred,n),
               "Error in mmsclean: resp cannot also be in pred",fixed=T)

  #test 4
  mats<-list(resp=matrix(1:9,3,3),pred1=matrix(10:18,3,3),pred2=matrix(5,4,4))
  resp<-1
  pred<-2:3
  n<-1
  expect_error(mmsclean(mats,resp,pred,n),
               "Error in mmsclean: all matrices must be same dimension and square",fixed=T)
  
  #test 5
  mats<-list(resp=matrix(1:9,3,3),pred1=matrix(10:18,3,3),pred2=matrix(19:27,3,3))
  resp<-1
  pred<-2:3
  n<-2
  expect_error(mmsclean(mats,resp,pred,n),
               "Error in mmsclean: n out of range",fixed=T)
})

test_that("test the output of mmsclean", {
  #test 1
  mats<-list(v1=matrix(1:9,3,3),v2=matrix(10:18,3,3),v3=matrix(19:27,3,3))
  resp<-2
  pred<-c(1,3)
  n<-1
  h<-mmsclean(mats,resp,pred,n)
  exp.ans<-list(mats=list(v2=matrix(c(NA,11,12,NA,NA,15,NA,NA,NA),3,3),
                          v1=matrix(c(NA,2,3,NA,NA,6,NA,NA,NA),3,3),
                          v3=matrix(c(NA,20,21,NA,NA,24,NA,NA,NA),3,3)),
                resp=1,pred=c(2,3))
  expect_equal(h,exp.ans)
})
