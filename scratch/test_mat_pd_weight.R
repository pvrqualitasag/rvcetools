#' ---
#' title: Testing the function make_pd_weight
#' date: "`r Sys.Date()`"
#' ---
#' 
#' ## Disclaimer
#' The bending approach described in Jorjani2003 is implemented in 
#' `make_pd_weight`. The following statements are used to test the 
#' function.
#' 
#' ## Preparation
#' We start by cleaning up all old objects
#+ clean-up
rm(list = ls())
#+ def-mat1
(mat_test_jorjani <- matrix(data = c(100,95,80,40,40,
                                    0.95,100,95,80,40,
                                    0.80,0.95,100,95,80,
                                    0.40,0.80,0.95,100,95,
                                    0.40,0.40,0.80,0.95,100), ncol = 5, byrow = TRUE))

#' The matrix [1] given in the paper is converted to a symmetric var-covar matrix
#+ mat-convert
mat_var_jorjani <- mat_test_jorjani
mat_var_jorjani[mat_var_jorjani < 1] <- 0
# mat_var_jorjani

tmat_var_jorjani <- t(mat_var_jorjani)
diag(tmat_var_jorjani) <- 0
mat_var_jorjani <- mat_var_jorjani + tmat_var_jorjani
eigen(mat_var_jorjani)

#' The matrix is written to a file in the package for later usage
# readr::write_delim(as.data.frame(mat_var_jorjani), 
#                    path = 'inst/extdata/mat_test_jorjani2003.dat', 
#                    col_names = FALSE)


#' ## Test
#' The function is unrolled and tested with the matrix in the paper
#+ func-unroll
A = mat_var_jorjani 
pmat_weight = matrix(1, nrow = nrow(A), ncol = ncol(A))
pn_eps = 1e-4
pn_max_iter = 1e5

#' Definition of variables
#+ var-def
mat_result <- A
# get eigenvalue/eigenvector decomposition
D <- eigen(mat_result)
# assign eigenvectors and eigen values to separate variables
vec_eval <- D$values
mat_evec <- D$vectors
# check whether mat_result must be bended
nnr_eval_below_eps <- sum(vec_eval < pn_eps)
cat(" * Number of eigenvalues below eps: ", nnr_eval_below_eps, "\n")
if (nnr_eval_below_eps < 1L)
  cat("Input matrix seams to be pdf, return")

#' Iteration
#+ bending-iter
nnr_iter <- 1
# loop condition
min(vec_eval) < pn_eps

# updating variables in current iteration
(vec_corr_eval <- vec_eval)
vec_corr_eval[which(vec_corr_eval < pn_eps)] <- pn_eps + sqrt(.Machine$double.eps)
vec_corr_eval
# compute the matrix of the current iteration
mat_result <- mat_result - (mat_result - mat_evec %*% diag(vec_corr_eval) %*% t(mat_evec)) * pmat_weight
mat_result
# get eigenvalue/eigenvector decomposition
D <- eigen(mat_result)
# assign eigenvectors and eigen values to separate variables
vec_eval <- D$values
mat_evec <- D$vectors
nnr_iter <- nnr_iter + 1

#' Use the function
#+ func-test
rvcetools::make_pd_weight(mat_var_jorjani)

#' ## Second Test using weight matrix
#' The paper gives also an example of the weight matrix
#+ def-weight-mat
mat_weight <- matrix(data = c(1000,500,20,50,200,
                              500, 1000,500,5,50,
                              20, 500, 1000,20,20,
                              50, 5, 20, 1000,200,
                              200, 50, 20, 200,1000), ncol = 5, byrow = TRUE)

mat_weight
isSymmetric(mat_weight)
mat_r_w <- rvcetools::make_pd_weight(mat_var_jorjani, pmat_weight = mat_weight)
mat_r_w
round(mat_r_w, digits = 1)
