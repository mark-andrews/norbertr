test_that("dchoice", {
  expect_equal(dchoice(0.5, 1, 0.5),
               1 - dchoice(0.5, 1, 0.5, choice = 0))
})
