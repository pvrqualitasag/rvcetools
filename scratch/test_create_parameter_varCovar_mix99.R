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

  
  
  
#rvcetools::create_parameter_varCovar_mix99(pl_mat,ps_output_file='mix99_beef_mandant.var',pbLog=FALSE) #! Something wrong -> debugging  
  
#  create_parameter_varCovar_mix99 <- function(pl_mat,
#                                              ps_output_file,
#                                              pbLog = FALSE){
    ps_output_file <- 'mix99_beef_mandant.var'
  
    # Prepare the different input to build the parameter file
    vec_randomEffect_name <- names(pl_mat)
    n_nr_randomEffect <- length(vec_randomEffect_name)
    
    ### with maternal effect
    vec_random_effect_req_Maternal <- c("animal|dam", "residual")
    ### with only direct effect
    vec_random_effect_req <- c("animal", "residual")
    
    # Check if in inputFile the random effects animal and residual are present
    if (all(vec_random_effect_req %in% vec_randomEffect_name)){
      # Get the other random effects
      vec_random_effects_mand <- setdiff(vec_randomEffect_name, vec_random_effect_req)
      # Animal and residual should have a specific order in mix99
      vec_random_effect_order <- c(vec_random_effects_mand, vec_random_effect_req)
    }else if(all(vec_random_effect_req_Maternal %in% vec_randomEffect_name)){
      # Get the other random effects
      vec_random_effects_mand <- setdiff(vec_randomEffect_name, vec_random_effect_req_Maternal)
      # Animal and residual should have a specific order in mix99
      vec_random_effect_order <- c(vec_random_effects_mand, vec_random_effect_req_Maternal)
    }else{
      stop(" * ERROR: Required random effects animal or animal|dam and residual are not both in list of random effects")
    }
    
    # Check if ps_output_file is existing
    if (file.exists(ps_output_file))
      file.remove(ps_output_file)
    
 
    #Build Variance/Covariance Parameter-File for Mix99
       vec_trait_name <- rownames(pl_mat[[1]])
       
       ### check if maternal trait are available
       idx_Maternal_trait_name <- grep("_maternal",vec_trait_name)
       if(!is.null(idx_Maternal_trait_name)){
         vec_trait_name_Maternal <- vec_trait_name
         n_nr_trait_Maternal <- length(vec_trait_name_Maternal)
         
         vec_trait_name <- vec_trait_name[-idx_Maternal_trait_name]
         n_nr_trait <- length(vec_trait_name)
       }else{
         ### only direct traits
         n_nr_trait <- dim(pl_mat[[1]])[1]
       }
       
       idx_rand_eff <- 1
       for(Z in vec_random_effect_order){
         if(Z=="animal|dam"){
           for(i in 1:n_nr_trait_Maternal){
             for(j in i:n_nr_trait_Maternal){
               cat(Z, vec_trait_name_Maternal[i], vec_trait_name_Maternal[j], format(pl_mat[[Z]][[i,j]], scientific = FALSE), file = ps_output_file, append = TRUE)
               cat("\n", sep= "", file = ps_output_file, append = TRUE)
             }
           }
         }else{
           for(i in 1:n_nr_trait){
             for(j in i:n_nr_trait){
               cat(Z, vec_trait_name[i], vec_trait_name[j], format(pl_mat[[Z]][[i,j]], scientific = FALSE), file = ps_output_file, append = TRUE)
               cat("\n", sep= "", file = ps_output_file, append = TRUE)
             }
           }
         }
         
         idx_rand_eff <- idx_rand_eff + 1
       }
    
#  }