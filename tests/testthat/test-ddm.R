test_that("dchoice", {
  expect_equal(dchoice(0.5, 1, 0.5),
               1 - dchoice(0.5, 1, 0.5, choice = 0))
})


test_that("simulate_ddm", {
  expect_equal(nrow(simulate_ddm(10, 0.5, 2, 1)), 10)
})
