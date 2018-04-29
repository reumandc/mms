context("mmsscore_int")

test_that("test mmsscore_int in a perfect-regression case", {
  set.seed(101)
  v2<-matrix(rnorm(100),10,10)
  v3<-matrix(rnorm(100),10,10)
  v4<-matrix(rnorm(100),10,10)
  v1<-1*v2+2*v3+3*v4+1
  mats<-list(v1=v1,v2=v2,v3=v3,v4=v4)
  pred<-2:4
  n<-2
  maxruns<-Inf
  h<-mmsscore_int(mats,pred,n,maxruns)
  expect_equal(h$lno.score,0)
  expect_equal(h$num.pos,choose(10,2))
  expect_equal(h$num.att,choose(10,2))
})

test_that("test mmsscore_int in an arbitrary test case that should come out the same on future runs", {
  set.seed(201)
  v2<-matrix(rnorm(100),10,10)
  v3<-matrix(rnorm(100),10,10)
  v4<-matrix(rnorm(100),10,10)
  v1<-1*v2+2*v3+3*v4+1+matrix(rnorm(100,sd=.1),10,10)
  mats<-list(v1=v1,v2=v2,v3=v3,v4=v4)
  pred<-2:4
  n<-2
  maxruns<-Inf
  h<-mmsscore_int(mats,pred,n,maxruns)
  #I got this hash with digest::digest(h)
  expect_known_hash(h,hash="7bc4275e3365541e02473d793293ae03")  
})

test_that("test mmsscore_int ancillary outputs", {
  set.seed(201)
  v2<-matrix(rnorm(100),10,10)
  v3<-matrix(rnorm(100),10,10)
  v4<-matrix(rnorm(100),10,10)
  v1<-1*v2+2*v3+3*v4+1+matrix(rnorm(100,sd=.1),10,10)
  mats<-list(v1=v1,v2=v2,v3=v3,v4=v4)
  pred<-2:4
  n<-2
  maxruns<-40
  h<-mmsscore_int(mats,pred,n,maxruns)
  expect_equal(h$num.pos,choose(10,2))
  expect_equal(h$num.att,maxruns)
  expect_lte(h$num.rnk,maxruns)
  expect_lte(h$num.usd,h$num.rnk)
})