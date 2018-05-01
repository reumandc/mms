context("mmsvarwts")

test_that("test error catching functionality of mmsvarwts", {
 #test 1
 pred<-c(2,3)
 prednames<-c("predictor1")
 h<-list(model.names=c("2","3","2:3"),
         freq.top=rpois(3,3)
 )
 expect_error(mmsvarwts(pred, h, prednames),
              "Error in mmsvarwts: prednames must be the same length as pred", fixed=T)
 #test 2
 pred<-c(2,3)
 prednames<-c("predictor1","predictor2")
 h<-list(model.names=c("2","3","2:3"))
 expect_error(mmsvarwts(pred, h, prednames),
              "Error in msvarwts: weights must contain model.names and freq.top", fixed=T)
 
})

test_that("further tests of mmsvarwts", {
  weights<-data.frame(model.names=c("2","3","2:3"),freq.top=c(100,200,300),stringsAsFactors = F)
  h<-mmsvarwts(pred=c(2,3),weights=weights)
  expect_equal(h$summed.weights[h$prednames=="2"],2/3)
  expect_equal(h$summed.weights[h$prednames=="3"],5/6)
  
  weights<-data.frame(model.names=c("2","3","2:3"),freq.top=c(100,200,300),stringsAsFactors = F)
  h<-mmsvarwts(pred=c(2,3),weights=weights)
  expect_equal(h$summed.weights[h$prednames=="2"],2/3)
  expect_equal(h$summed.weights[h$prednames=="3"],5/6)

  weights<-data.frame(model.names=c("2","3","4","5","c(2,4)","2:5"),
                      freq.top=c(100,200,100,500,250,600),stringsAsFactors = F)
  h<-mmsvarwts(pred=c(2,3,4,5),weights=weights)
  expect_equal(h$summed.weights[h$prednames=="2"],(100+250+600)/sum(weights$freq.top))  
  expect_equal(h$summed.weights[h$prednames=="3"],(200+600)/sum(weights$freq.top))  
  expect_equal(h$summed.weights[h$prednames=="4"],(100+250+600)/sum(weights$freq.top))  
})
