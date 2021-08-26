#Rscript -e "rvcetools::create_parameter_varCovar_mix99(pl_mat=rvcetools::build_matrix(rvcetools::read_vce('VCE_results.csv')),ps_output_file='mix99_dairy_motherbreed.var',pbLog=FALSE)"
#Parsed with column specification:
#cols(
#  type = col_character(),
#  traits = col_character(),
#  random_effect = col_character(),
#  estimate = col_double(),
#  STD_ERR_estimate = col_double(),
#  model_name = col_character(),
#  data_subset_number = col_character(),
#  used_seed = col_character(),
#  trait_combination = col_character(),
#  log_transformation = col_character(),
#  optimization_status = col_double()
#)
#Fehler in `[<-`(`*tmp*`, smry_Z$trait[i], smry_Z$surrogate[i], value = smry_Z$meanEstimate[i]) : 
#  Indizierung außerhalb der Grenzen
#Ruft auf: <Anonymous> -





ps_input_tibble <- rvcetools::read_vce('../../work/VCE_results_GA_Dairy_Mandant.csv')

#pl_mat <- rvcetools::build_matrix(ps_input_tibble)  #! Something wrong -> debugging

#build_matrix <- function(ps_input_tibble){
library(dplyr)
library(tidyr)
  
  ### # Build Matrix
  # Get variance and covariance informations in a tibble
  tbl_varCovar <- ps_input_tibble %>% filter(type == "variance" | type == "covariance") %>% select(type,traits,random_effect,estimate)
  # Split traits into trait 1 and trait 2, some records have only 1 trait, which causes `separate()` to issue a warning which 
  # is suppressed here
  suppressWarnings( tbl_varCovar <- tbl_varCovar %>% separate(traits, c('trait', 'surrogate'), remove = FALSE, sep ="([+])") )
  
  #Here was the bug, solved by adding sep ="([+])" in the separate-function
  
  
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
  
  resultList <- list()
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
#}
