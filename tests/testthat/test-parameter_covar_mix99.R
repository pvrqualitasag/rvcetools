context("Create Variance Covariance Parameter File for MiX99")
library(rvcetools)


test_that("MiX99 Parameter Schaeffer", {
  sInputFile <- system.file("extdata","VCE_results.csv", package = "rvcetools")
  # bending method of Schaeffer without rounding
  s_result_schaeffer <- system.file("extdata","par_varCovar_mix99_schaeffer.txt", package = "rvcetools")
  tbl_result_schaeffer <- readr::read_delim(file = s_result_schaeffer, delim = ' ', col_names = FALSE)
  s_out_schaeffer <- file.path(tempdir(), "par_varCovar_mix99_schaeffer.txt")
  # call the main function to generate the parameter file
  parameter_varCovar_mix99(ps_input_file = sInputFile, 
                           ps_output_file = s_out_schaeffer)
  # compare results to what is expected
  tbl_out_schaeffer <- readr::read_delim(file = s_out_schaeffer, delim = ' ', col_names = FALSE)
  file.remove(s_out_schaeffer)
  expect_equal(tbl_result_schaeffer, tbl_out_schaeffer)

  # bending method of Schaeffer with rounding
  s_result_schaeffer_d3 <- system.file("extdata","par_varCovar_mix99_schaeffer_d3.txt", package = "rvcetools")
  tbl_result_schaeffer_d3 <- readr::read_delim(file = s_result_schaeffer_d3, delim = ' ', col_names = FALSE)
  s_out_schaeffer_d3 <- file.path(tempdir(), "par_varCovar_mix99_schaeffer_d3.txt")
  # call the main function to generate the parameter file
  parameter_varCovar_mix99(ps_input_file = sInputFile, 
                           ps_output_file = s_out_schaeffer_d3,
                           pn_digits = 3)
  
  # compare results to what is expected
  tbl_out_schaeffer_d3 <- readr::read_delim(file = s_out_schaeffer_d3, delim = ' ', col_names = FALSE)
  file.remove(s_out_schaeffer_d3)
  expect_equal(tbl_result_schaeffer_d3, tbl_out_schaeffer_d3)
  
})


test_that("MiX99 Parameter Ratio", {
  sInputFile <- system.file("extdata","VCE_results.csv", package = "rvcetools")
  # bending method of Ratio without rounding
  s_result_ratio <- system.file("extdata","par_varCovar_mix99_ratio.txt", package = "rvcetools")
  tbl_result_ratio <- readr::read_delim(file = s_result_ratio, delim = ' ', col_names = FALSE)
  s_out_ratio <- file.path(tempdir(), "par_varCovar_mix99_ratio.txt")
  # call the main function to generate the parameter file
  parameter_varCovar_mix99(ps_input_file  = sInputFile, 
                           ps_output_file = s_out_ratio,
                           pn_ratio       = 100)
  # compare results to what is expected
  tbl_out_ratio <- readr::read_delim(file = s_out_ratio, delim = ' ', col_names = FALSE)
  file.remove(s_out_ratio)
  expect_equal(tbl_result_ratio, tbl_out_ratio)
  
  # bending method of ratio with rounding
  s_result_ratio_d3 <- system.file("extdata","par_varCovar_mix99_ratio_d3.txt", package = "rvcetools")
  tbl_result_ratio_d3 <- readr::read_delim(file = s_result_ratio_d3, delim = ' ', col_names = FALSE)
  s_out_ratio_d3 <- file.path(tempdir(), "par_varCovar_mix99_ratio_d3.txt")
  # call the main function to generate the parameter file
  parameter_varCovar_mix99(ps_input_file = sInputFile, 
                           ps_output_file = s_out_ratio_d3,
                           pn_ratio       = 100,
                           pn_digits      = 3)
  
  # compare results to what is expected
  tbl_out_ratio_d3 <- readr::read_delim(file = s_out_ratio_d3, delim = ' ', col_names = FALSE)
  file.remove(s_out_ratio_d3)
  expect_equal(tbl_result_ratio_d3, tbl_out_ratio_d3)
  
})


test_that("MiX99 Parameter Unweighted", {
  sInputFile <- system.file("extdata","VCE_results.csv", package = "rvcetools")
  # bending method of Unweighted without rounding
  s_result_unweighted <- system.file("extdata","par_varCovar_mix99_unweighted.txt", package = "rvcetools")
  tbl_result_unweighted <- readr::read_delim(file = s_result_unweighted, delim = ' ', col_names = FALSE)
  s_out_unweighted <- file.path(tempdir(), "par_varCovar_mix99_unweighted.txt")
  # call the main function to generate the parameter file
  parameter_varCovar_mix99(ps_input_file  = sInputFile, 
                           ps_output_file = s_out_unweighted,
                           pn_eps         = 1e-4)
  # compare results to what is expected
  tbl_out_unweighted <- readr::read_delim(file = s_out_unweighted, delim = ' ', col_names = FALSE)
  file.remove(s_out_unweighted)
  expect_equal(tbl_result_unweighted, tbl_out_unweighted)
  
  # bending method of unweighted with rounding
  s_result_unweighted_d3 <- system.file("extdata","par_varCovar_mix99_unweighted_d3.txt", package = "rvcetools")
  tbl_result_unweighted_d3 <- readr::read_delim(file = s_result_unweighted_d3, delim = ' ', col_names = FALSE)
  s_out_unweighted_d3 <- file.path(tempdir(), "par_varCovar_mix99_unweighted_d3.txt")
  # call the main function to generate the parameter file
  parameter_varCovar_mix99(ps_input_file = sInputFile, 
                           ps_output_file = s_out_unweighted_d3,
                           pn_eps         = 1e-4,
                           pn_digits      = 3)
  
  # compare results to what is expected
  tbl_out_unweighted_d3 <- readr::read_delim(file = s_out_unweighted_d3, delim = ' ', col_names = FALSE)
  file.remove(s_out_unweighted_d3)
  expect_equal(tbl_result_unweighted_d3, tbl_out_unweighted_d3)
  
})
