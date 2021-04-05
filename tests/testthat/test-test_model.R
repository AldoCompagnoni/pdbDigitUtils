data(baer_pdb)

test_that("test_model returns correct strings when model is ready", {
  
  x <- test_model(baer_pdb, "aaa312")
  
  expect_equal(x, "Test passed, model is ready :)")
  
})


test_that("test_model returns correct values when missing targets", {
  
  x <- test_model(baer_pdb, "aaa314")
  
  expect_match(x, "No test target found")
  
})