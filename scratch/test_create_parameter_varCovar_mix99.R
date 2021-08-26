### #
### # Test following functions for maternal effects:
### #                         read_vce
### #                         build_matrix
### #                         create_parameter_varCovar_mix99
### # ----------------------------------------------------------


### # test read_vce function
ps_input_tibble <- rvcetools::read_vce("../../work/VCE_results_GA_Beef_Mandant.csv")

### # test build_matrix function
#pl_mat <- rvcetools::build_matrix(ps_input_tibble) #! Something wrong -> debugging
### # debugging build_matrix
library(dplyr)
library(tidyr)

#build_matrix <- function(ps_input_tibble){
  
  ### # Build Matrix
  # Get variance and covariance informations in a tibble
  tbl_varCovar <- ps_input_tibble %>% filter(type == "variance" | type == "covariance") %>% select(type,traits,random_effect,estimate)
  
  ### # Since here is not working for maternal-traits
    # Split traits into trait 1 and trait 2, some records have only 1 trait, which causes `separate()` to issue a warning which 
  # is suppressed here
  suppressWarnings( tbl_varCovar <- tbl_varCovar %>% separate(traits, c('trait', 'surrogate'), remove = FALSE, sep ="([+])") )
  
  
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
  vec_randomEffect_name <- unique(smry$random_effect)

  ### for maternal effect the matrix of the variance is only larger for animal|dam
  if(is.element("animal|dam",vec_randomEffect_name)){
    n_nr_trait_withMaternal <- length(vec_trait_name)
    mat_randomEffect_withMaternal <- matrix(0, nrow = n_nr_trait_withMaternal, ncol = n_nr_trait_withMaternal)
    rownames(mat_randomEffect_withMaternal) <- vec_trait_name
    colnames(mat_randomEffect_withMaternal) <- vec_trait_name
    
    # remove maternal traits and consider only the direct traits
    idx_Maternal_trait_name <- grep("_maternal",vec_trait_name)
    vec_trait_name_Direct <- vec_trait_name[-idx_Maternal_trait_name]
    n_nr_trait <- length(vec_trait_name_Direct)
    mat_randomEffect <- matrix(0, nrow = n_nr_trait, ncol = n_nr_trait)
    rownames(mat_randomEffect) <- vec_trait_name_Direct
    colnames(mat_randomEffect) <- vec_trait_name_Direct
    
  }else{
    ### only direct effects
    n_nr_trait <- length(vec_trait_name)
    mat_randomEffect <- matrix(0, nrow = n_nr_trait, ncol = n_nr_trait)
    rownames(mat_randomEffect) <- vec_trait_name
    colnames(mat_randomEffect) <- vec_trait_name
  }
  

  resultList <- list()
  for(Z in vec_randomEffect_name){
    
    if(Z=="animal|dam"){
      # take only values for a random effect
      smry_Z <- smry %>% filter(random_effect == Z)
      
      # loop over rows of tbl and write elements to matrix
      for (i in 1:nrow(smry_Z)){
        mat_randomEffect_withMaternal[smry_Z$trait[i], smry_Z$surrogate[i]] <- smry_Z$meanEstimate[i]
        mat_randomEffect_withMaternal[smry_Z$surrogate[i], smry_Z$trait[i]] <- smry_Z$meanEstimate[i]
      }
      resultList[[Z]] <- mat_randomEffect_withMaternal
      
    }else{
      # take only values for a random effect
      smry_Z <- smry %>% filter(random_effect == Z)
      
      # loop over rows of tbl and write elements to matrix
      for (i in 1:nrow(smry_Z)){
        mat_randomEffect[smry_Z$trait[i], smry_Z$surrogate[i]] <- smry_Z$meanEstimate[i]
        mat_randomEffect[smry_Z$surrogate[i], smry_Z$trait[i]] <- smry_Z$meanEstimate[i]
      }
      resultList[[Z]] <- mat_randomEffect
    }
    
  }
  
  ### # Result of matrix as list
  return(resultList)
#}

