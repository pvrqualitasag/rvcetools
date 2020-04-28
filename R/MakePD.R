#' ---
#' title: Check and Transform to Insure Positive Definite of Matrix
#' date:  "`r Sys.Date()`"
#' ---

# -- Bending Functions ------------------------------------------ ###

#' @title Bending of Matrix A
#'
#' @description
#' The input matrix A is decomposed into its eigen-values and eigen-vectors,
#' The negative eigen-values are projected into the range between zero and
#' the smallest positive eigen-value.
#'
#' @param A input matrix
#' @param pn_digits number of digits to be rounded to
#'
#' @return Bended positive-definite matrix A
#' @export makePD2
#'
#' @examples
#' G<- matrix(c(100,80,20,6,80,50,10,2,20,10,6,1,6,2,1,1), ncol = 4, byrow=TRUE)
#' makePD2(G)
makePD2 <- function(A, pn_digits = NULL){
  # compute eigenvalue-eigenvector decomposition
  D  <-  eigen(A)
  # assign separate values
  V <- D$values
  U <- D$vectors
  # return orignal matrix, if A is positive definite
  if (min(V) > 0)
    return(A)
  # get the dimensions of A
  N <- nrow(A)
  # determine number of negative eigenvalues and sum twice the negative ev
  nneg <- sum(V < 0)
  vec_neg_ev <- V[which(V < 0)]
  sr <- 2 * sum(vec_neg_ev)
  # compute the weight factor wr and determine the smallest positive ev
  wr = (sr*sr*100+1)
  p = V[N - nneg]
  # correct the negative eigenvalues
  V[which(V < 0)] <- p * (sr - vec_neg_ev) * (sr - vec_neg_ev) / wr
  # reconstruct A from eval-evec-decomposition and return
  if (is.null(pn_digits)){
    result_mat <- U %*% diag(V) %*% t(U)
  } else {
    result_mat <- round(U %*% diag(V) %*% t(U), digits = pn_digits)
    if (min(eigen(result_mat, only.values = TRUE)$values) < 0)
      stop(" * ERROR: makePD2: result matrix not positive definite after rounding")
  }
  # issue a warning, if smallest eigenvalue of bended matrix is very small
  if (min(eigen(result_mat, only.values = TRUE)$values) < sqrt(.Machine$double.eps))
    warning(" * WARNING: makePD2: Smallest eigenvalue less than: ", 
            sqrt(.Machine$double.eps), 
            " => use alternative bending method.")
  # add row and col-names back to result matrix
  colnames(result_mat) <- colnames(A)
  rownames(result_mat) <- rownames(A)
  return(result_mat)
}


#' @title Bending Matrix A Based On Ratio Of Eigenvalues
#'
#' @description
#' ## Parametrisation
#' The current implementation bends the given matrix in a fixed fashion. All it does is to increment the negative
#' eigenvalues between 0 and the smallest positive eigenvalue. If the smallest positive eigenvalue is very small, then
#' the corrected eigenvalues will be about 100 times smaller which corresponds to the hard-coded factor in the computation
#' of the weight `wr`.
#'
#' For some applications it might be interesting to be able to specify a ratio between the largest and the smallest
#' eigenvalue. With that it should be possible to scale all eigenvalues to be within that ratio boundary.
#'
#' ## Ratio of Eigenvalues
#' Instead of just changing the negative eigenvalues by mapping them between zero and the smallest eigenvalue,
#' we would like to correct eigenvalues such that all of them are poitive and such that the ratio between the
#' largest and the smallest eigenvalue is below a certain threshold.
#'
#' In principle this approach can be implemented similarly to the one that corrects the negative eigenvalues,
#' presented so far. The only adaptation that one must do is to replace the limit of the eigenvalues that must
#' be changed from 0 to that number determined by the maximum ration. Everything else should stay the same,
#' modulo a special treatment of any negative eigenvalues. An easy solution to that might be to do it in two
#' steps, first make all eigenvalues positive using an approach used in `makePD2()`. Then we can use a second
#' step to come up with a correction that ensures a maximum ration between eigenvalues. On the other hand it
#' should be possible to provide a one step solution.
#'
#' The function is called `make_pd_rat_ev()`. As arguments the function takes the input matrix and a maximum
#' ratio between largest and smallest eigenvalue.
#' @param A input matrix
#' @param pn_max_ratio max ratio
#' @param pn_digits number of digits to be rounded to
#'
#' @return Bended positive-definite matrix A with ratio
#' @export make_pd_rat_ev
make_pd_rat_ev <- function(A, pn_max_ratio, pn_digits = NULL){
  # get eigenvalue/eigenvector decomposition
  D <- eigen(A)
  # assign eigenvectors and eigen values to separate variables
  vec_eval <- D$values
  mat_evec <- D$vectors
  # number of negative eigenvalues
  nneg <- sum(vec_eval < 0)
  # correction based on ratio of max and minimum of the absolute eigenvalues
  max_ev <- max(vec_eval)
  n_abs_rat <- max_ev / min(abs(vec_eval))
  # if the ratio is ok and no negative eigenvalues, then we can return the input matrix
  if (n_abs_rat < pn_max_ratio && nneg < 1)
    return(A)
  # in case we have to correct them, we correct all eigenvalues that are negative or
  # outside the ratio boundary. Find eigenvalue that must be corrected due to the ratio boundary
  vec_idx_ev_out <- which(max_ev/vec_eval > pn_max_ratio)
  # add those which are negative
  if (nneg > 0) vec_idx_ev_out <- unique(c(vec_idx_ev_out, which(vec_eval < 0)))
  # the boundary for the smallest ev is
  n_min_ev <- max_ev / pn_max_ratio
  # the range between the last eigenvalue that does not need correction (n_last_ev_not_corrected)
  # and the smallest eigenvalue of the input matrix is projected to the range between the
  # n_last_ev_not_corrected and the minimum eigenvalue after correction based on max desired ratio
  n_last_ev_not_corrected <- vec_eval[vec_idx_ev_out[1]-1]
  n_out_range <- max(n_last_ev_not_corrected - vec_eval[vec_idx_ev_out])
  # the corrected range between the last eigenvalue inside the range and the ratio boundary is
  n_corr_range <- n_last_ev_not_corrected - n_min_ev
  n_range_rat <- n_corr_range / n_out_range
  # correct according to the range ratios
  vec_eval[vec_idx_ev_out] <- n_last_ev_not_corrected - (n_last_ev_not_corrected - vec_eval[vec_idx_ev_out]) * n_range_rat
  # reconstruct matrix
  if (is.null(pn_digits)){
    result_mat <-mat_evec %*% diag(vec_eval) %*% t(mat_evec)
  } else {
    result_mat <- round(mat_evec %*% diag(vec_eval) %*% t(mat_evec), digits = pn_digits)
    if (min(eigen(result_mat, only.values = TRUE)$values) < 0)
      stop(" * ERROR: make_pd_rat_ev: result matrix not positive definite after rounding")
  }
  # issue a warning, if smallest eigenvalue of bended matrix is very small
  if (min(eigen(result_mat, only.values = TRUE)$values) < sqrt(.Machine$double.eps))
    warning(" * WARNING: make_pd_rat_ev: Smallest eigenvalue less than: ", 
            sqrt(.Machine$double.eps), 
            " => increase ratio or use alternative bending method.")
  # add row and col-names back to result matrix
  colnames(result_mat) <- colnames(A)
  rownames(result_mat) <- rownames(A)
  # return reconstructed matrix
  return(result_mat)

}


#' @title Weighted Bending of a Matrix
#' 
#' @description 
#' This function implements the algorithm described in Jorjani2003. Without 
#' specifying a weight matrix, it corresponds to the unweighted bending where 
#' each eigenvalue is set to a lower limit which is given by the parameter pn_eps.
#' The advantage of this bending method is that it is possible to specify a 
#' weight matrix where matrix elements with a high weight do not get changed as 
#' much as elements with a small weight. The number of observations used to 
#' estimate each variance component can be used as a weighting factor.
#'
#' @param A input matrix
#' @param pmat_weight weight matrix
#' @param pn_eps smallest accepted eigenvalue
#' @param number of digits to be rounded to
#'
#' @return mat_result bended matrix
#' @export make_pd_weight
#'
#' @examples
#' # reading input matrix from paper by Jorjani et al 2003
#' sinput_file <- system.file('extdata', 'mat_test_jorjani2003.dat', package = 'rvcetools')
#' mat_test_jorjani <- as.matrix(readr::read_delim(file = sinput_file, delim = ' ', col_names = FALSE))
#' make_pd_weight(mat_test_jorjani)
make_pd_weight <- function(A, 
                           pmat_weight = matrix(1, nrow = nrow(A), ncol = ncol(A)), 
                           pn_eps      = 1e-4,
                           pn_max_iter = 1e6, 
                           pn_digits   = NULL){
  # intializse result matrix
  mat_result <- A
  # get eigenvalue/eigenvector decomposition
  D <- eigen(mat_result)
  # assign eigenvectors and eigen values to separate variables
  vec_eval <- D$values
  mat_evec <- D$vectors
  # check whether mat_result must be bended
  nnr_eval_below_eps <- sum(vec_eval < pn_eps)
  if (nnr_eval_below_eps < 1L)
    return(mat_result)
  
  # repeat as long as smallest eigenvalue is smaller than pn_eps
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
  if (!is.null(pn_digits)){
    mat_result <- round(mat_result, digits = pn_digits)    
    if (min(eigen(mat_result, only.values = TRUE)$values) < 0)
      stop(" * ERROR: make_pd_weight: result matrix not positive definite after rounding")
  }
  # issue a warning, if smallest eigenvalue of bended matrix is very small
  if (min(eigen(mat_result, only.values = TRUE)$values) < sqrt(.Machine$double.eps))
    warning(" * WARNING: make_pd_weight: Smallest eigenvalue less than: ", 
            sqrt(.Machine$double.eps), 
            " => change weight or use alternative bending method.")
  # add row and col-names back to result matrix
  colnames(mat_result) <- colnames(A)
  rownames(mat_result) <- rownames(A)
  # return result
  return(mat_result)
  
}



# -- Bending List of Matrices ------------------------------------------ ###

#' @title Bending of a List of Matrices for Different Random Effects
#' 
#' @description 
#' Given a list of variance-covariance matrices passed by the parameter \code{pl_mat}, 
#' each of the matrices is checked whether it is positive definite based on the smallest 
#' eigenvalue of the matrix. Non-positive definite matrices are bent using a method 
#' that is determined based on the parameters given. If the function is called with 
#' just the list of matrices, then the Schaeffer method is used using the function 
#' \code{makePD2()}. Specifing a ratio between largest and smallest eigenvalue using 
#' the parameter \code{pn_ratio} causes the usage of the ratio method. When specifying 
#' a minimal eigenvalue using parameter \code{pn_eps}, the method of Jorjani et al. 
#' 2003 is used. This method exists in a weighted and an unweighted version. Which 
#' of the two versions is used is determined by the parameter pmat_weight. When a 
#' weight matrix is given via \code{pmat_weight} the weighted version is used, otherwise 
#' the unweighted version is used.
#' 
#' @param pl_mat list of input matrices which are bent if necessary
#' @param pn_ratio specified ratio between largest and smallest eigenvalue 
#' @param pn_eps smallest eigenvalue in weighted bending
#' @param pmat_weight weight matrix for weighted bending
#' @param pn_digits  number of digits to be rounded to
#' @param pb_log indicator flag for logging
#' @param plogger log4r object to log to
#'
#' @export check_transform_positivedefinit
check_transform_positivedefinit <- function(pl_mat,
                                            pn_ratio    = NULL,
                                            pn_eps      = NULL,
                                            pmat_weight = NULL,
                                            pn_digits    = NULL,
                                            pb_log       = FALSE,
                                            plogger     = NULL){
 
  ### # setup of loging
  if (pb_log) {
    if (is.null(plogger)){
      lgr <- get_rvce_logger(ps_logfile = 'check_transform_positivedefinit.log', ps_level = 'INFO')
    } else {
      lgr <- plogger
    }
    rvce_log_info(lgr, 'check_transform_positivedefinit', 
                  paste0('Started function with parameters: \n * pn_ratio: ', pn_ratio, '\n',
                         ' * pn_eps: ', pn_eps, '\n',
                         ' * pn_digits: ', pn_digits, '\n', collapse = ''))
  }
  
  ### # get names of random effects from names of list of input variance-covariance matrices
  vec_randomEffect_name <- names(pl_mat)
  ### # Check if matrix is positive definite
  PDresultList <- list()
  for(Z in vec_randomEffect_name){
    # log message on which random effect is used
    if (pb_log)
      rvce_log_info(lgr, 'check_transform_positivedefinit',
                    paste0('Checking variance-covariance matrix for effect: ', Z))

    # compute eigenvalue decomposition
    D  <-  eigen(pl_mat[[Z]])
    # assign separate values
    V <- D$values
    # determine if matrix pl_mat[[Z]] has negative eigenvalues
    if(sum(V < 0) < 1){
      if (pb_log)
        rvce_log_info(lgr, 'check_transform_positivedefinit',
                      paste0('No negative eigenvalue found: ', sum(V < 0)))
      ### # no negative eigenvalues are available, do rounding if required
      if (is.null(pn_digits)){
        PDresultList[[Z]] <- pl_mat[[Z]]
      } else {
        PDresultList[[Z]] <- round(pl_mat[[Z]], digits = pn_digits)
      }

    } else {
      ### # at least one eigenvalue is negative, so matrix must be bent, 
      ### #  method is chosen based on parameters.
      if(is.null(pn_ratio) && is.null(pn_eps)){
        if (pb_log)
          rvce_log_info(lgr, 'check_transform_positivedefinit', 'Bending with Schaeffer')
        # Run function makePD2 which is an vectorized version of Schaeffer
        PDresultList[[Z]] <- makePD2(pl_mat[[Z]], pn_digits = pn_digits)
      } else {
        ### # either bending based on eigenvalue ratios or based on minimal eigenvalue 
        ### #  (weighted or unweighted) is chosen here
        if (is.null(pn_eps)){
          if (pb_log)
            rvce_log_info(lgr, 'check_transform_positivedefinit', 
                          paste0('Bending with eigenvalue ratio: ', pn_ratio))
          # Run function make_pd_rat_ev with parameter pn_ratio
          PDresultList[[Z]] <- make_pd_rat_ev(pl_mat[[Z]], pn_max_ratio = pn_ratio, pn_digits = pn_digits)
        } else {
          ### # in case no weight matrix is specified, then matrix is bent based on a lower bound of eigenvalues 
          if (is.null(pmat_weight)){
            if (pb_log)
              rvce_log_info(lgr, 'check_transform_positivedefinit', 
                            paste0('Unweighted bending with minimal eigenvalue: ', pn_eps))
            PDresultList[[Z]] <- make_pd_weight(pl_mat[[Z]], pn_eps = pn_eps, pn_digits = pn_digits)
          } else {
            ### # case of weighted bending
            if (pb_log)
              rvce_log_info(lgr, 'check_transform_positivedefinit', 
                            paste0('Weighted bending with minimal eigenvalue: ', pn_eps))
            PDresultList[[Z]] <- make_pd_weight(pl_mat[[Z]], 
                                                pn_eps = pn_eps, 
                                                pmat_weight = pmat_weight, 
                                                pn_digits = pn_digits)
          }
        }
      }
    }
  }

  ### # Result of matrix as list, which is positive definite
  return(PDresultList)
}
