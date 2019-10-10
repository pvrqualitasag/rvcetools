#' ---
#' title: Constructing multivariate estimates from analyses by parts in a matrix
#' date:  "`r Sys.Date()`"
#' ---

#' @title Convert Variance-Covariance Stored in Tibble to a List of matrices
#'
#' @description
#' Storing the variance and covariances in the tibble read from the input file
#' in a list of matrices.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom tidyr separate
#' @importFrom dplyr group_by
#' 
#' @param psInputFile tibble containing all variance-covariance components
#' @export build_matrix
#' 
#' @examples 
#' sInputFile <- system.file("extdata","VCE_results.csv", package = "rvcetools")
#' tbl_vce <- read_vce(ps_input_file = sInputFile)
#' l_mat <- build_matrix(psInputFile = tbl_vce)
build_matrix <- function(psInputFile,
                         pbLog = FALSE){

  ### # Build Matrix
  # Get variance and covariance informations in a tibble
  tbl_varCovar <- psInputFile %>% filter(type == "variance" | type == "covariance") %>% select(type,traits,random_effect,estimate)
  # Split traits into trait 1 and trait 2, some records have only 1 trait, which causes `separate()` to issue a warning which 
  # is suppressed here
  suppressWarnings( tbl_varCovar <- tbl_varCovar %>% separate(traits, c('trait', 'surrogate'), remove = FALSE) )
  tbl_varCovar[is.na(tbl_varCovar$surrogate),'surrogate'] <- tbl_varCovar[is.na(tbl_varCovar$surrogate),'trait']
  # Change order of trait and surrogate based on alphabetic order of them
  idx <- tbl_varCovar[,'trait'] > tbl_varCovar[,'surrogate']
  vec_tmp_trait <- tbl_varCovar[idx,'trait']
  tbl_varCovar[idx,'trait'] <- tbl_varCovar[idx,'surrogate']
  tbl_varCovar[idx,'surrogate'] <- vec_tmp_trait$trait
  # For each traits&random_effect get the mean value of all variants
  by_grp <- tbl_varCovar %>% group_by(trait, surrogate, random_effect)
  smry <- summarise(by_grp,
                    meanEstimate = mean(estimate, na.rm = TRUE))
  # Prepare matrix to be able to read from tbl
  vec_trait_name <- unique(smry$trait)
  n_nr_trait <- length(vec_trait_name)
  mat_randomEffect <- matrix(0, nrow = n_nr_trait, ncol = n_nr_trait)
  rownames(mat_randomEffect) <- vec_trait_name
  colnames(mat_randomEffect) <- vec_trait_name
  vec_randomEffect_name <- unique(smry$random_effect)

  resultList <- NULL
  for(Z in vec_randomEffect_name){
    # take only values for a random effect
    smry_Z <- smry %>% filter(random_effect == Z)

    # loop over rows of tbl and write elements to matrix
    for (i in 1:nrow(smry_Z)){
      mat_randomEffect[smry_Z$trait[i], smry_Z$surrogate[i]] <- smry_Z$meanEstimate[i]
      mat_randomEffect[smry_Z$surrogate[i], smry_Z$trait[i]] <- smry_Z$meanEstimate[i]
    }
    resultList[[Z]] <- mat_randomEffect
  }

  ### # Result of matrix as list
  return(resultList)

}
