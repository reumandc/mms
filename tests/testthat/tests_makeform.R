context("makeform")

test_that("test the function",{
  mats<-list(resp=1,pred1=2,pred2=3,pred3=4)
  expect_equal(makeform(mats),"resp~pred1+pred2+pred3")
})