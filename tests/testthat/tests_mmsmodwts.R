context("mmsmodwts")

test_that("test mmssmodwts in an arbitrary test case that should come out the same on future runs", {
  old.seed<-.Random.seed
  set.seed(201)
  v2<-matrix(rnorm(100),10,10)
  v2<-v2+t(v2)
  v3<-matrix(rnorm(100),10,10)
  v3<-v3+t(v3)
  v4<-matrix(rnorm(100),10,10)
  v4<-v4+t(v4)
  err<-matrix(rnorm(100,sd=.1),10,10)
  err<-err+t(err)
  v1<-1*v2+2*v3+3*v4+1+err
  mats<-list(v1=v1,v2=v2,v3=v3,v4=v4)
  model.names<-NA
  nrand<-25
  n<-2
  maxruns<-40
  h<-mmsmodwts(mats=mats,model.names=model.names,nrand=nrand,n=n,maxruns=maxruns,progress=F)
  #This should have freq.top larger for 2,3,4 than for other models. I checked that
  #visually for this seed and it worked, so now just make sure future runs give
  #the same answer. Hash below was obtained using digest::digest(h).
  expect_known_hash(h,hash="43094ac579ed7fc1d3d1db026b9d3703")   

  #test ancillary outputs
  expect_equal(h$num.pos,rep(nrand*choose(10,2),7))
  expect_equal(h$num.att,rep(nrand*maxruns,7))
  expect_equal(h$num.rnk<=h$num.att,rep(T,7))
  expect_equal(h$num.usd<=h$num.rnk,rep(T,7))
  .Random.seed<-old.seed
})  

