context("makenames")

test_that("tests of makenames",{
  expect_equal(makenames(3),list(2,3,c(2,3)))
  
  h<-makenames(5)
  #I got this hash with digest::digest(h)
  expect_known_hash(h,hash="b5701653525b70d179ebfea0681aca2a")   
})
