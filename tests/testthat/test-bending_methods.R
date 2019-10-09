context("Bending Methods")
library(rvcetools)

test_that("Identity matrix", {
  mat_iden <- diag(1, nrow = 5)
  expect_equal(makePD2(mat_iden), mat_iden)
  expect_equal(make_pd_rat_ev(mat_iden, pn_max_ratio = 100), mat_iden)
  expect_equal(make_pd_weight(mat_iden), mat_iden)
})
