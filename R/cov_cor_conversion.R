### #
### #
### #
### #   Purpose:   Conversion between Variance-Covariance and Correlation
### #   started:   2019-10-02 (pvr)
### #
### # ##################################################################### ###

#' Conversion of covariances to correlations
#'
#' @description 
#' Use the function `base::cov2cor()` to convert a variance-covariance 
#' matrix to a correlation matrix
#' 
#' @param V symmetric positive definite variance-covariance matrix
#'
#' @return correlation matrix
#' @export cov_to_cor
#'
#' @examples
#' ## Taken from ?cov2cor: Correlation Matrix of Multivariate sample:
#' (Cv <- cov(longley))
#' stopifnot(all.equal(cov2cor(Cv), cor(longley)))
cov_to_cor <- function(V){
  return(cov2cor(V))
}


#' Conversion of Correlation Matrix and Vector of Variances to Variance-Covariance Matrix
#'
#' @description 
#' The same approach as in `base::cov2cor()` is used to compute the variance-covariance 
#' matrix from a matrix of correlations and a vector of variance values.
#' 
#' @param pmat_cor correlation matrix
#' @param pvec_var vector of variance components
#'
#' @return variance covariance matrix
#' @export cor_to_cov
#'
#' @examples
#' ## Taken from ?cov2cor: Correlation Matrix of Multivariate sample:
#' Cv <- cov(longley)
#' Cl <- cor(longley)
#' stopifnot(all.equal(cor_to_cov(Cl, diag(Cv)), Cv))
cor_to_cov <- function(pmat_cor, pvec_var){
  p <- (d <- dim(pmat_cor))[1L]
  if (!is.numeric(pmat_cor) || length(d) != 2L || p != d[2L] || p != length(pvec_var)) 
    stop("'pmat_cor' is not a square numeric matrix or the vector of variance does not have the appropriate length")
  Is <- sqrt(pvec_var)
  if (any(!is.finite(Is))) 
    warning("pvec_var had 0 or NA entries; non-finite result is doubtful")
  r <- pmat_cor
  r[] <- Is * pmat_cor * rep(Is, each = p)
  r[cbind(1L:p, 1L:p)] <- pvec_var
  r
}
