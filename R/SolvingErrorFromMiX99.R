###
###
###
###   Purpose:   Solving Error Message from MiX99: VC matrix non positive definite
###   started:   2021/03/12 (skn)
###
### ######################################### ###

#' @title Solving Message "VC matrix non positive definite" from MiX99 
#' 
#' @description 
#' In the software MiX99 of Luke can the message "VC matrix non positive definite" poping up.
#' To solve this issue, the VC file non positive definite can be used as input and
#' a solution will be proposed as output file.
#' 
#' @importFrom dplyr filter
#' 
#' @param ps_input_file input file with VC in MiX99
#' @param pn_ratio specified ratio between largest and smallest eigenvalue 
#' @param pn_eps smallest eigenvalue in weighted bending
#' @param pmat_weight weight matrix for weighted bending
#' @param pn_digits  number of digits to be rounded to
#' @param ps_output_file solved file with VC for MiX99
#'
#'
#' @export solving_errorFromMiX99_VCnotPD
solving_errorFromMiX99_VCnotPD <- function(ps_input_file,
                                           pn_ratio    = NULL,
                                           pn_eps      = NULL,
                                           pmat_weight = NULL,
                                           pn_digits    = NULL,
                                           ps_output_file){
  
  if(!file.exists(ps_input_file)){
    stop(" * ERROR: Required VC file for MiX99 is not existing!")
  }
  ### # Read MiX99 VC file
  tbl_vc <- readr::read_delim(file = ps_input_file, col_names = FALSE,delim = " ")
  
  
  ### # Prepare matrix to be able to read from tbl [part of function build_matrix()]
  vec_trait_name <- unique(tbl_vc$X2)
  n_nr_trait <- length(vec_trait_name)
  mat_randomEffect <- matrix(0, nrow = n_nr_trait, ncol = n_nr_trait)
  rownames(mat_randomEffect) <- vec_trait_name
  colnames(mat_randomEffect) <- vec_trait_name
  vec_randomEffect_name <- unique(tbl_vc$X1)
  
  resultList <- list()
  for(Z in vec_randomEffect_name){
    # take only values for a random effect
    smry_Z <- tbl_vc %>% filter(X1 == Z)
    # loop over rows of tbl and write elements to matrix
    for (i in 1:nrow(smry_Z)){
      mat_randomEffect[smry_Z$X2[i], smry_Z$X3[i]] <- smry_Z$X4[i]
      mat_randomEffect[smry_Z$X3[i], smry_Z$X2[i]] <- smry_Z$X4[i]
    }
    resultList[[Z]] <- mat_randomEffect
  }
  
  
  ### # Transfrom Matrix to insure beeing Positive Definit [part of function check_transform_positivedefinit()]
  # Can not use check_transform_positivedefinit() ->! BUG: vec_randomEffect_name <- names(pl_mat)
  pl_mat <- resultList
  ### # Check if matrix is positive definite
  PDresultList <- list()
  for(Z in vec_randomEffect_name){
    # compute eigenvalue decomposition
    D  <-  eigen(pl_mat[[Z]])
    # assign separate values
    V <- D$values
    if(sum(V < 0) < 1){
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
        # Run function makePD2 which is an vectorized version of Schaeffer
        PDresultList[[Z]] <- makePD2(pl_mat[[Z]], pn_digits = pn_digits)
      } else {
        ### # either bending based on eigenvalue ratios or based on minimal eigenvalue 
        ### #  (weighted or unweighted) is chosen here
        if (is.null(pn_eps)){
          # Run function make_pd_rat_ev with parameter pn_ratio
          PDresultList[[Z]] <- make_pd_rat_ev(pl_mat[[Z]], pn_max_ratio = pn_ratio, pn_digits = pn_digits)
        } else {
          ### # in case no weight matrix is specified, then matrix is bent based on a lower bound of eigenvalues 
          if (is.null(pmat_weight)){
            PDresultList[[Z]] <- make_pd_weight(pl_mat[[Z]], pn_eps = pn_eps, pn_digits = pn_digits)
          } else {
            ### # case of weighted bending
            PDresultList[[Z]] <- make_pd_weight(pl_mat[[Z]], 
                                                pn_eps = pn_eps, 
                                                pmat_weight = pmat_weight, 
                                                pn_digits = pn_digits)
          }
        }
      }
    }
  }
  
  
  ### # Build Parameter-File in txt-Format with Variances for Mix99 [part of function create_parameter_varCovar_mix99()]
  pl_mat_out <- PDresultList
  idx_rand_eff <- 1
  for(Z in vec_randomEffect_name){
    for(i in 1:n_nr_trait){
      for(j in i:n_nr_trait){
        cat(Z, vec_trait_name[i], vec_trait_name[j], format(pl_mat_out[[Z]][[i,j]], scientific = FALSE), file = ps_output_file, append = TRUE)
        cat("\n", sep= "", file = ps_output_file, append = TRUE)
      }
    }
    idx_rand_eff <- idx_rand_eff + 1
  }
  
  
}
