
devtools::document();devtools::install()

sInputFile <- system.file("extdata","VCE_results.csv", package = "rvcetools")

# no rounding
rvcetools::parameter_varCovar_mix99(ps_input_file = sInputFile, 
                                    ps_output_file = 'par_varCovar_mix99_schaeffer.txt', 
                                    pb_log = TRUE)


rvcetools::parameter_varCovar_mix99(ps_input_file  = sInputFile, 
                                    ps_output_file = 'par_varCovar_mix99_ratio.txt',
                                    pn_ratio       = 100,
                                    pb_log         = TRUE)

rvcetools::parameter_varCovar_mix99(ps_input_file  = sInputFile, 
                                    ps_output_file = 'par_varCovar_mix99_unweighted.txt', 
                                    pn_eps         = 1e-4,
                                    pb_log = TRUE)

# rounding
rvcetools::parameter_varCovar_mix99(ps_input_file  = sInputFile, 
                                    ps_output_file = 'par_varCovar_mix99_schaeffer_d3.txt', 
                                    pn_digits      = 3,
                                    pb_log         = TRUE)


rvcetools::parameter_varCovar_mix99(ps_input_file  = sInputFile, 
                                    ps_output_file = 'par_varCovar_mix99_ratio_d3.txt',
                                    pn_ratio       = 100,
                                    pn_digits      = 3,
                                    pb_log         = TRUE)

rvcetools::parameter_varCovar_mix99(ps_input_file  = sInputFile, 
                                    ps_output_file = 'par_varCovar_mix99_unweighted_d3.txt', 
                                    pn_eps         = 1e-4,
                                    pn_digits      = 3,
                                    pb_log         = TRUE)


# debug unweighted with rounding
sInputFile <- system.file("extdata","VCE_results.csv", package = "rvcetools")
tbl_vce <- rvcetools::read_vce(ps_input_file = sInputFile)
l_mat <- rvcetools::build_matrix(psInputFile = tbl_vce)
l_mat

# stepping through
A = l_mat$animal 
pmat_weight = matrix(1, nrow = nrow(A), ncol = ncol(A))
pn_eps      = 1e-4
pn_max_iter = 1e6 
pn_digits   = 3

mat_result <- A
# get eigenvalue/eigenvector decomposition
D <- eigen(mat_result)
# assign eigenvectors and eigen values to separate variables
vec_eval <- D$values
mat_evec <- D$vectors
# check whether mat_result must be bended
(nnr_eval_below_eps <- sum(vec_eval < pn_eps))

nnr_iter <- 1
while (min(vec_eval) < pn_eps){
  # check number of iterations
  if (nnr_iter > pn_max_iter)
    stop(" *** make_pd_weight: Maximum number of iterations reached")
  # replace all eigenvalues that are smaller than pn_eps by pn_eps
  vec_corr_eval <- vec_eval
  # eigenvalues that are below the limit of pn_eps are replaced by pn_eps plus a small quantity 
  #  which is required otherwise the while condition does not stop
  vec_corr_eval[which(vec_corr_eval < pn_eps)] <- pn_eps + sqrt(.Machine$double.eps)
  # compute the matrix of the current iteration
  mat_result <- mat_result - (mat_result - mat_evec %*% diag(vec_corr_eval) %*% t(mat_evec)) * 1/pmat_weight
  # get eigenvalue/eigenvector decomposition
  D <- eigen(mat_result)
  # assign eigenvectors and eigen values to separate variables
  vec_eval <- D$values
  mat_evec <- D$vectors
  nnr_iter <- nnr_iter + 1
}
mat_result
!is.null(pn_digits)
(mat_result <- round(mat_result, digits = pn_digits) )
min(eigen(mat_result, only.values = TRUE)$values) < 0
min(eigen(mat_result, only.values = TRUE)$values) < sqrt(.Machine$double.eps)
colnames(mat_result) <- colnames(A)
rownames(mat_result) <- rownames(A)
mat_result
