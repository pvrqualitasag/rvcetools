context("Bending Methods")
library(rvcetools)

test_that("Identity matrix", {
  mat_iden <- diag(1, nrow = 5)
  expect_equal(makePD2(mat_iden), mat_iden)
  expect_equal(make_pd_rat_ev(mat_iden, pn_max_ratio = 100), mat_iden)
  expect_equal(make_pd_weight(mat_iden), mat_iden)
})

# Tests using the bending method from the paper Jorjani et al 2003
test_that("Jorjani matrix", {
  # input matrix from Jorjani 2003 paper
  mat_jor_input <- matrix(data = c(100,95,80,40,40,
                                   95,100,95,80,40,
                                   80,95,100,95,80,
                                   40,80,95,100,95,
                                   40,40,80,95,100), ncol = 5)
  # weight matrix from Jorjani 2003 paper
  mat_jor_weight_matrix <- matrix(data = c(1000,500,20,50,200,
                                           500, 1000,500,5,50,
                                           20, 500, 1000,20,20,
                                           50, 5, 20, 1000,200,
                                           200, 50, 20, 200,1000), ncol = 5)
  # result of unweighted bending verified with paper
  mat_jor_unweighted <- matrix(data = c(103.16918,  90.83313,  79.47122,  44.53731,  37.07254, 
                                        90.83313, 106.49734,  94.18961,  74.07039,  44.53731, 
                                        79.47122,  94.18961, 102.31355,  94.18961,  79.47122, 
                                        44.53731,  74.07039,  94.18961, 106.49734,  90.83313, 
                                        37.07254,  44.53731,  79.47122,  90.83313, 103.16918) , ncol = 5)  
  # result of weighted bending verified with paper
  mat_jor_weighted <- matrix(data = c(100.16230,  94.51688,  82.93198,  43.57901, 39.17851,
                                       94.51688, 100.62484,  93.98071,  60.02275, 45.85467,
                                       82.93198,  93.98071, 100.69569,  84.90972, 73.14282,
                                       43.57901,  60.02275,  84.90972, 100.30649, 94.22843,
                                       39.17851,  45.85467,  73.14282,  94.22843, 100.18022) , ncol = 5)
  # check computation result of make_pd_weight
  mat_jor_result <- make_pd_weight(mat_jor_input)
  expect_true(sum((mat_jor_result - mat_jor_unweighted)^2) < sqrt(.Machine$double.eps))
  # check pdf of result
  expect_true(min(eigen(mat_jor_result, only.values = TRUE)$values) > 0)
  # check weighted version
  mat_jor_result_weighted <- make_pd_weight(mat_jor_input, pmat_weight = mat_jor_weight_matrix)
  expect_true(sum((mat_jor_result_weighted - mat_jor_weighted)^2) < sqrt(.Machine$double.eps))
  # check pdf of weighted result
  expect_true(min(eigen(mat_jor_result_weighted, only.values = TRUE)$values) > 0)
})

# Bending method of the Schaeffer book
test_that("Schaeffer method", {
  # input matrix from Schaeffer Chpt 7.4 pg 123
  mat_input_schaeffer <- matrix(data = c(100,80,20,6,
                                          80,50,10,2,
                                          20,10,6,1,
                                          6,2,1,1), ncol = 4)
  # result matrix from book
  mat_ben_schaeffer <- matrix(data = c(103.615081, 75.499246, 18.377481, 5.013141,
                                       75.499246,  55.603411, 12.020026, 3.228634,
                                       18.377481,  12.020026, 6.728218, 1.442922,
                                       5.013141,   3.228634,  1.442922, 1.269397), ncol = 4)
  # bend input matrix and compare to result
  mat_result_schaeffer <- makePD2(mat_input_schaeffer)
  expect_true(sum((mat_result_schaeffer - mat_ben_schaeffer)^2) < sqrt(.Machine$double.eps))
  # result matrix should be pdf
  expect_true(min(eigen(mat_result_schaeffer, only.values = TRUE)$values) > 0)
})

# Bending based on ratio between largest and smallest eigen value
test_that("Ratio method", {
  # using the input matrix from Schaeffer
  mat_input_schaeffer <- matrix(data = c(100,80,20,6,
                                         80,50,10,2,
                                         20,10,6,1,
                                         6,2,1,1), ncol = 4)
  # result matrix,
  mat_ben_ratio <- matrix(data = c(104.195930, 74.774864, 18.0837820, 4.9954987,
                                    74.774864, 56.506986, 12.3914112, 3.2288164,
                                    18.083782, 12.391411,  7.0139582, 0.8658603,
                                     4.995499,  3.228816  ,0.8658603, 3.7720338), ncol = 4)
  # compare function output to expected result
  mat_ratio_result <- make_pd_rat_ev(mat_input_schaeffer, pn_max_ratio = 100)
  expect_true(sum((mat_ratio_result - mat_ben_ratio)^2) < sqrt(.Machine$double.eps))
  # check pdf of bended matrix
  expect_true(min(eigen(mat_ratio_result, only.values = TRUE)$values) > 0)
})