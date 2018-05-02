context("matregtest")

test_that("test matregtest in a perfect-regression case", {
  old.seed<-.Random.seed
  set.seed(101)
  v2<-matrix(rnorm(100),10,10)
  v2<-v2+t(v2)
  v3<-matrix(rnorm(100),10,10)
  v3<-v3+t(v3)
  v4<-matrix(rnorm(100),10,10)
  v4<-v4+t(v4)
  v1<-1*v2+2*v3+3*v4+1
  mats<-list(v1=v1,v2=v2,v3=v3,v4=v4)
  pred<-2:4
  drop<-4
  numperm<-10
  h<-matregtest(mats,pred,drop,numperm)
  expect_equal(h$ssr_dat,0)
  expect_equal(h$p,0)
  .Random.seed<-old.seed
})

test_that("test matregtest error catching", {
  old.seed<-.Random.seed
  set.seed(101)
  v2<-matrix(rnorm(100),10,10)
  v2<-v2+t(v2)
  v3<-matrix(rnorm(100),10,10)
  v3<-v3+t(v3)
  v4<-matrix(rnorm(100),10,10)
  v4<-v4+t(v4)
  v1<-1*v2+2*v3+3*v4+1
  mats<-list(v1=v1,v2=v2,v3=v3,v4=v4)
  pred<-2:3
  drop<-4
  numperm<-10
  expect_error(matregtest(mats,pred,drop,numperm),"Error in matregtest: drop should be a subset of pred")
  .Random.seed<-old.seed
})

test_that("test matregtest in arbitrary test cases that should come out the same on future runs", {
  old.seed<-.Random.seed
  set.seed(201)
  v2<-matrix(rnorm(100),10,10)
  v2<-v2+t(v2)
  v3<-matrix(rnorm(100),10,10)
  v3<-v3+t(v3)
  v4<-matrix(rnorm(100),10,10)
  v4<-v4+t(v4)
  v5<-matrix(rnorm(100),10,10)
  v5<-v5+t(v5)
  err<-matrix(rnorm(100,sd=.05),10,10)
  err<-err+t(err)
  v1<-1*v2+2*v3+1+err
  mats<-list(v1=v1,v2=v2,v3=v3,v4=v4,v5=v5)
  pred<-2:5
  drop<-4:5
  numperm<-100
  h<-matregtest(mats,pred,drop,numperm)
  #I got this hash with digest::digest(h)
  expect_known_hash(h,hash="4a1352751f6e936ff58e1d17fa92c5c6")
  
  drop<-2:3
  h<-matregtest(mats,pred,drop,numperm)
  #I got this hash with digest::digest(h)
  expect_known_hash(h,hash="a17b5db17284e14f7c8c8e8224a7c9a3")
  .Random.seed<-old.seed
})
