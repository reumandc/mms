context("mmsrank")

test_that("test error catching functionality of mmsrank", {
  #test 1
  set.seed(101)
  v2<-matrix(rnorm(100),10,10)
  v3<-matrix(rnorm(100),10,10)
  v4<-matrix(rnorm(100),10,10)
  v1<-1*v2+2*v3+3*v4+1
  mats<-list(v1=v1,v2=v2,v3=v3,v4=v4)
  model.names<-list(2:4,3:4,1:4)
  n<-3
  maxruns<-NA
  rank.mod<-T
  expect_error(mmsrank(mats=mats,model.names=model.names,n=n,maxruns=maxruns,rank.mod=rank.mod),
               "Error in mmsrank: listed models cannot include the response",fixed=T)
})  
  
test_that("test mmsrank in an arbitrary test case that should come out the same on future runs", {
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
  h<-mmsrank(mats=mats,model.names=model.names,n=n,maxruns=maxruns,rank.mod=rank.mod)
  #I got this hash with digest::digest(h)
  expect_known_hash(h,hash="92774925e3dd8ab181afb7396563ab02")    
})

test_that("test mmsrank for a semi-realistic case", {
  set.seed(401)
  v2<-matrix(rnorm(100),10,10)
  v3<-matrix(rnorm(100),10,10)
  v4<-matrix(rnorm(100),10,10)
  v1<-1*v2+2*v3+3*v4+1+matrix(rnorm(100,sd=.1),10,10)
  mats<-list(v1=v1,v2=v2,v3=v3,v4=v4)
  model.names<-NA
  n<-2
  maxruns<-NA
  rank.mod<-T
  h<-mmsrank(mats=mats,model.names=model.names,n=n,maxruns=maxruns,rank.mod=rank.mod)
  #This should tend to have:
  # 1) the full model (2:4) as top ranked
  # 2) for a given number of included predictors, models with larger predictor numbers should be 
  #     favored since the coefficients used were larger for those
  # 3) models with certain predictors should tend to be better than models with subsets of those
  #All of these were true for this seed when I looked at the output. So just error check whether
  #the same output is obtained on future runs.
  expect_known_hash(h,hash="69ca4d2b705a3f20a77adbab738b1391")    
})