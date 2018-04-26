context("matregtest")

test_that("test error catching functionality of matregtest", {
  mats<-list(v1=matrix(1:16,4,4),v2=matrix(17:32,4,4),v3=matrix(33:48,4,4))
  resp<-4
  pred<-c(2,3)
  drop<-2
  numperm<-100
  expect_error(matregtest(mats,resp,pred,drop,numperm),
               "Error in matregtest: response variable index out of bounds",fixed=T)

  resp<-1
  pred<-c()
  drop<-2
  numperm<-100
  expect_error(matregtest(mats,resp,pred,drop,numperm)
               ,"Error in matregtest: at least one value required in pred")
  
  resp<-1
  pred<-2:4
  drop<-2
  numperm<-100
  expect_error(matregtest(mats,resp,pred,drop,numperm),
               "Error in matregtest: predictor variable index out of bounds",fixed=T)
  
  resp<-1
  pred<-1:3
  drop<-2
  numperm<-100
  expect_error(matregtest(mats,resp,pred,drop,numperm),
               "Error in matregtest: resp cannot also be in pred",fixed=T)
  
  resp<-1
  pred<-2
  drop<-3
  numperm<-100
  expect_error(matregtest(mats,resp,pred,drop,numperm),
               "Error in matregtest: drop should be a subset of pred")
  
  mats<-list(v1=matrix(1:16,4,4),v2=matrix(17:32,4,4),v3=matrix(5,5,5))
  resp<-1
  pred<-2:3
  drop<-3
  numperm<-100
  expect_error(matregtest(mats,resp,pred,drop,numperm),
               "Error in matregtest: all matrices must be same dimension and square",fixed=T)
})

test_that("test matregtest in a perfect-regression case", {
  set.seed(101)
  v2<-matrix(rnorm(100),10,10)
  v3<-matrix(rnorm(100),10,10)
  v4<-matrix(rnorm(100),10,10)
  v1<-1*v2+2*v3+3*v4+1
  mats<-list(v1=v1,v2=v2,v3=v3,v4=v4)
  resp<-1
  pred<-2:4
  drop<-4
  numperm<-10
  h<-matregtest(mats,resp,pred,drop,numperm)
  expect_equal(h$ssr_dat,0)
  expect_equal(h$p,0)
})

test_that("test matregtest in arbitrary test cases that should come out the same on future runs", {
  set.seed(201)
  v2<-matrix(rnorm(100),10,10)
  v3<-matrix(rnorm(100),10,10)
  v4<-matrix(rnorm(100),10,10)
  v5<-matrix(rnorm(100),10,10)
  v1<-1*v2+2*v3+1+matrix(rnorm(100,sd=.1),10,10)
  mats<-list(v1=v1,v2=v2,v3=v3,v4=v4,v5=v5)
  resp<-1
  pred<-2:5
  drop<-4:5
  numperm<-100
  h<-matregtest(mats,resp,pred,drop,numperm)
  #I got this hash with digest::digest(h)
  expect_known_hash(h,hash="2957ef227e2f894546436b8d8580b44a")
  
  drop<-2:3
  h<-matregtest(mats,resp,pred,drop,numperm)
  #I got this hash with digest::digest(h)
  expect_known_hash(h,hash="f9e2c2a7cf0db590cfbaa850c418979d")
})
