context("mmsvarwts")

test_that("test error catching functionality of mssvarwts", {
 #test 1
 pred<-c(2,3)
 varnames<-c("response","predictor1")
 h<-list(model.names=c("2","3","2:3"),
         freq.top=rpois(3,3)
 )
 expect_error(mmsvarwts(pred, h, varnames),
              "Error in mmsvarwts: varnames must be one element longer than pred", fixed=T)
 #test 2
 pred<-c(2,3)
 varnames<-c("response","predictor1","predictor2")
 h<-list(model.names=c("2","3","2:3"))
 expect_error(mmsvarwts(pred, h, varnames),
              "Error in msvarwts: weights must contain model.names and freq.top", fixed=T)
 
})

test_that("test mmwvarwts in an arbitrary test case that should ome out the same on future runs", {
  set.seed(5117)
  pred<-c(2,3)
  varnames<-c("response", "predictor1", "predictor2")
  wts<-list(model.names=c("2","3","2:3"),
          freq.top=rpois(3,3))
  h<-mmsvarwts(pred, wts, varnames)
  expect_known_hash(h, hash="ebcf5e84286c5f297e07b61221b52a0c")        
})
