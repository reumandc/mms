context("mmsrank_int")
library(mms)

test_that("test mmsrank_int in an arbitrary test case that should come out the same on future runs", {
  set.seed(201)
  v2<-matrix(rnorm(100),10,10)
  v3<-matrix(rnorm(100),10,10)
  v4<-matrix(rnorm(100),10,10)
  v1<-1*v2+2*v3+3*v4+1+matrix(rnorm(100,sd=.1),10,10)
  mats<-list(v1=v1,v2=v2,v3=v3,v4=v4)
  model.names<-list(c(2,3),c(3,4))
  n<-2
  maxruns<-100
  rank.mod<-T
  h<-mmsrank_int(mats=mats,model.names=model.names,n=n,maxruns=maxruns,rank.mod=rank.mod)
  #I got this hash with digest::digest(h)
  expect_known_hash(h,hash="a114eab35c7fff7b74efe6f1afe50f95")    
})

test_that("test mmsrank_int for a semi-realistic case", {
  set.seed(401)
  v2<-matrix(rnorm(100),10,10)
  v3<-matrix(rnorm(100),10,10)
  v4<-matrix(rnorm(100),10,10)
  v1<-1*v2+2*v3+3*v4+1+matrix(rnorm(100,sd=.1),10,10)
  mats<-list(v1=v1,v2=v2,v3=v3,v4=v4)
  model.names<-list(2,3,4,c(2,3),c(2,4),c(3,4),c(2,3,4))
  n<-2
  maxruns<-Inf
  rank.mod<-T
  h<-mmsrank_int(mats=mats,model.names=model.names,n=n,maxruns=maxruns,rank.mod=rank.mod)
  #This should tend to have:
  # 1) the full model (2:4) as top ranked
  # 2) for a given number of included predictors, models with larger predictor numbers should be 
  #     favored since the coefficients used were larger for those
  # 3) models with certain predictors should tend to be better than models with subsets of those
  #All of these were true for this seed when I looked at the output. So just error check whether
  #the same output is obtained on future runs.
  expect_known_hash(h,hash="2d4617739698782f00a0f1c0c9f1ecea")    
})

test_that("test ancillary output for mmsrank_int",{
  set.seed(401)
  v2<-matrix(rnorm(100),10,10)
  v3<-matrix(rnorm(100),10,10)
  v4<-matrix(rnorm(100),10,10)
  v1<-1*v2+2*v3+3*v4+1+matrix(rnorm(100,sd=.1),10,10)
  mats<-list(v1=v1,v2=v2,v3=v3,v4=v4)
  model.names<-list(2,3,4,c(2,3),c(2,4),c(3,4),c(2,3,4))
  n<-2
  maxruns<-40
  rank.mod<-T
  h<-mmsrank_int(mats=mats,model.names=model.names,n=n,maxruns=maxruns,rank.mod=rank.mod)
  expect_equal(h$num.pos,rep(choose(10,2),7))
  expect_equal(h$num.att,rep(maxruns,7))
  expect_equal(h$num.rnk<=rep(maxruns,7),rep(T,7))
  expect_equal(h$num.usd<=h$num.rnk,rep(T,7))
})

