# testpredict.mars.R
library(mars)
load("testpredict.RData")
test_that("predict.mars() returns the correct object", {
  expect_equal(predict.mars(testmars,newdata=marstestdata), testpredict)
})
