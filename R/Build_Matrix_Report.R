#' ---
#' title: "Building a matrix from VCE results for the report"
#' date:  "2021-12-21"
#' #' ---
#'
#'
#' @title Build a matrix with VCE results for a report
#'
#' @description
#' Build a matrix with VCE results for a report
#'
#' @importFrom tidyr separate
#'
#' @param ps_random_effect random effect (for exemple: animal, herdyear, residual)
#' @param ps_offdiagonal covariance or genetic_correlation
#' @param ps_diagonal variance or heritability
#' @param pvec_vce_results_file_path path to csv-file with vce results
#' @return mat_randomEffect resulting matrix with vce results from the random effect
#'
#' @export build_vce_mat_report
build_vce_mat_report <- function(ps_random_effect,
                                 ps_offdiagonal,
                                 ps_diagonal,
                                 pvec_trait_name,
                                 pvec_vce_results_file_path){
  
  library(tidyr)
  
  # check if file exist
  if (!file.exists(pvec_vce_results_file_path))
    stop(" *** * ERROR in build_vce_mat: Cannot find vce file: ", pvec_vce_results_file_path)
  
  # read file
  tbl_vce <-  readr::read_delim(file = pvec_vce_results_file_path, delim = ";")
  
  #  select ps_random_effect in the file
  if("random_effect" %in% colnames(tbl_vce)){
    if(any(tbl_vce[["random_effect"]] == ps_random_effect)){
      tbl_vce <- tbl_vce[tbl_vce[["random_effect"]] == ps_random_effect,]
    }else{
      stop(" *** * ERROR in build_vce_mat: Cannot find in the file the random_effect: ", ps_random_effect)
    }
  }
  
  #  select ps_diagonal and ps_offdiagonal in the file
  if("type" %in% colnames(tbl_vce))
    tbl_vce <- tbl_vce[tbl_vce[["type"]] == ps_diagonal | tbl_vce[["type"]] == ps_offdiagonal,]
  
  # Split traits into trait 1 and trait 2, some records have only 1 trait, which causes `separate()` to issue a warning which is suppressed here
  suppressWarnings( tbl_vce <- tbl_vce %>% separate(traits, c('trait', 'surrogate'), remove = FALSE, sep ="([+])") )
  # if surrogate or trait have NA's, that get info from trait into surrogate column
  tbl_vce[is.na(tbl_vce$surrogate),'surrogate'] <- tbl_vce[is.na(tbl_vce$surrogate),'trait']
  
  # Prepare matrix to be able to read from tbl
  n_nr_trait <- length(pvec_trait_name)
  mat_randomEffect <- matrix(0, nrow = n_nr_trait, ncol = n_nr_trait)
  rownames(mat_randomEffect) <- pvec_trait_name
  colnames(mat_randomEffect) <- pvec_trait_name
  tbl_vce$estimate <- round(tbl_vce$estimate, 3)
  
  # loop over rows of tbl and write elements to matrix
  for (i in 1:nrow(tbl_vce)){
    mat_randomEffect[tbl_vce$trait[i], tbl_vce$surrogate[i]] <- tbl_vce$estimate[i]
    mat_randomEffect[tbl_vce$surrogate[i], tbl_vce$trait[i]] <- tbl_vce$estimate[i]
  }
  
  # Hide lower triangle
  mat_randomEffect<-mat_randomEffect
  mat_randomEffect[lower.tri(mat_randomEffect, diag=FALSE)]<-""
  mat_randomEffect<-as.matrix(mat_randomEffect)
  mat_randomEffect
  
  # result
  return(mat_randomEffect)
  
}
