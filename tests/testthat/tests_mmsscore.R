context("mmsscore")

test_that("test error catching functionality of mmsscore", {
  #test 1
  mats<-list(v1=matrix(1:1000,100,100),v2=matrix(1:1000,100,100),v3=matrix(1:1000,100,100))
  resp<-1
  pred<-2:3
  n<-10
  maxruns<-NA
  expect_error(mmsscore(mats,resp,pred,n,maxruns),
               "Error in mmsscore: more LNOs than the max integer, try reducing n or using maxruns",fixed=T)
})

test_that("test a perfect-regression case", {
  set.seed(101)
  v2<-matrix(rnorm(100),10,10)
  v3<-matrix(rnorm(100),10,10)
  v4<-matrix(rnorm(100),10,10)
  v1<-1*v2+2*v3+3*v4+1
  mats<-list(v1=v1,v2=v2,v3=v3,v4=v4)
  resp<-1
  pred<-2:4
  n<-2
  maxruns<-NA
  h<-mmsscore(mats,resp,pred,n,maxruns)
  expect_equal(h$lno.score,0)
  expect_equal(h$num.pos,choose(10,2))
  expect_equal(h$num.att,choose(10,2))
})

test_that("an arbitrary test case should come out the same on future runs", {
  set.seed(201)
  v2<-matrix(rnorm(100),10,10)
  v3<-matrix(rnorm(100),10,10)
  v4<-matrix(rnorm(100),10,10)
  v1<-1*v2+2*v3+3*v4+1+matrix(rnorm(100,sd=.1),10,10)
  mats<-list(v1=v1,v2=v2,v3=v3,v4=v4)
  resp<-1
  pred<-2:4
  n<-2
  maxruns<-NA
  h<-mmsscore(mats,resp,pred,n,maxruns)
  #I got this hash with digest::digest(h)
  expect_known_hash(h,hash="7bc4275e3365541e02473d793293ae03")  
})

