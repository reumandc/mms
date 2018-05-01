context("transmn")

test_that("tests for transmn error checking",{
  expect_error(transmn(list(2:3,c(1,4,5)),"num"),
               "Error in transmn: inform must be 'char' or 'numvect'")
  expect_error(transmn(list(2:3,c(4,5,6),"notnum"),"numvect"),
               "Error in intvect2char: input should be a list of numeric vectors")
  expect_error(transmn(list(2:3,2,3,4,2.001),"numvect"),
               "Error in intvect2char: input should be whole numbers >= 2")
  expect_error(transmn(list(1,2,3),"numvect"),
               "Error in intvect2char: input should be whole numbers >= 2")
})

test_that("tests for transmn functionality",{
  hn<-list(2,3,4,5,2:3,c(2,4),c(2,5),3:4,c(3,5),4:5,2:4,c(2,3,5),c(2,4:5),3:5,2:5)
  hc<-transmn(hn,"numvect")
  expect_equal(hn,transmn(hc,"char"))
})