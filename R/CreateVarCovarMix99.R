###
###
###
###   Purpose:   Build Parameter with Variances and Covariances for Mix99
###   started:   2019/09/30 (skn)
###
### ######################################### ###


#' @title Build Parameter with Variances and Covariances for Mix99
#' 
#' 
#' @param pl_mat list of matrices
#' @param ps_output_file output file to which MiX99 parameters should be written to
#' @param pbLog flag for logging
#'
#'
#' @export create_parameter_varCovar_mix99
create_parameter_varCovar_mix99 <- function(pl_mat,
                                            ps_output_file,
                                            pbLog = FALSE){

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

  # Build Variance/Covariance Parameter-File for Mix99
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
    ### with maternal effects
    if(Z=="animal|dam"){
      for(i in 1:n_nr_trait_Maternal){
        for(j in i:n_nr_trait_Maternal){
          cat(Z, vec_trait_name_Maternal[i], vec_trait_name_Maternal[j], format(pl_mat[[Z]][[i,j]], scientific = FALSE), file = ps_output_file, append = TRUE)
          cat("\n", sep= "", file = ps_output_file, append = TRUE)
        }
      }
    }else{
      ### only direct effects
      for(i in 1:n_nr_trait){
        for(j in i:n_nr_trait){
          cat(Z, vec_trait_name[i], vec_trait_name[j], format(pl_mat[[Z]][[i,j]], scientific = FALSE), file = ps_output_file, append = TRUE)
          cat("\n", sep= "", file = ps_output_file, append = TRUE)
        }
      }
    }
    
    idx_rand_eff <- idx_rand_eff + 1
  }

}



#' @title Parameter File for Mix99
#'
#' @description Read csv-file of the variance component estimates and construct
#' a matrix with this estimates by parts. This matrix is checked if it is
#' positive definite or not. If this matrix is not positive definite, 2
#' functions may be called. If you put the paramter psOptionRatio = FALSE,
#' makePD2() is called . For the parameter psOptionRatio = TRUE the function
#' make_pd_rat_ev() is called. The difference is, that with make_pd_rat_ev() you
#' have the option to whish a maximum ratio between largest and smallest
#' eigenvalue.
#'
#' The software mix99 need a paramter-File with the variance and covariance. So
#' this function is building this paramter-file.
#'
#' @param ps_input_file input csv-file
#' @param ps_output_file output txt-file for the bended (co-)variances
#' @param ps_raw_output_file output txt-file for the raw (co-)variances
#' @param pn_ratio  maximum ratio between largest and smallest eigenvalue, determines whether ratio method is used or not
#' @param pn_eps lower limit of smallest eigenvalue
#' @param pmat_weight weight matrix for weighted bending
#' @param pn_digits number of digits to be rounded to
#' @param pb_log indicator whether logs should be produced
#' @param plogger log4r logger object
#'
#' @examples
#' \dontrun{
#' # use test input csv-file
#' sInputFile <- system.file("extdata","VCE_results.csv", package = "rvcetools")
#' # bend matrices using Schaeffer method
#' parameter_varCovar_mix99(ps_input_file = sInputFile, ps_output_file = 'par_varCovar_mix99_schaeffer.txt')
#' # bend matrices using ratio method
#' parameter_varCovar_mix99(ps_input_file  = sInputFile, 
#'                          ps_output_file = 'par_varCovar_mix99_ratio.txt',
#'                          pn_ratio       = 100)
#' # unweighted bending with minimum eigenvalue
#' parameter_varCovar_mix99(ps_input_file  = sInputFile, 
#'                          ps_output_file = 'par_varCovar_mix99_unweighted.txt',
#'                          pn_eps         = 1e-4)
#' }
#' @export parameter_varCovar_mix99
parameter_varCovar_mix99 <- function(ps_input_file,
                                     ps_output_file,
                                     ps_raw_output_file = NULL,
                                     pn_ratio           = NULL,
                                     pn_eps             = NULL,
                                     pmat_weight        = NULL,
                                     pn_digits          = NULL,
                                     pb_log             = FALSE,
                                     plogger            = NULL){

  if (pb_log){
    if (is.null(plogger)){
      lgr <- get_rvce_logger(ps_logfile = 'parameter_varCovar_mix99.log',
                             ps_level   = 'INFO')
    } else {
      lgr <- plogger
    }
    rvce_log_info(lgr, 'parameter_varCovar_mix99',
                  paste0('Starting function with parameters:\n * ps_input_file: ', ps_input_file, '\n',
                         ' * ps_output_file: ', ps_output_file, '\n',
                         ' * pn_ratio: ', pn_ratio, '\n',
                         ' * pn_eps: ', pn_eps, '\n',
                         ' * pn_digits: ', pn_digits, collapse = ''))
  }


  ### # Read the results of variance component estimations
  if (pb_log) {
    rvce_log_info(lgr, 'parameter_varCovar_mix99',
                  paste0('Reading input from file: ', ps_input_file, '\n'))
  }
  RawVCTibble <- read_vce(ps_input_file = ps_input_file,
                          pb_log        = pb_log,
                          plogger       = lgr)
  if (pb_log) {
    rvce_log_info(lgr, 'parameter_varCovar_mix99',
                  paste0('Number of records read: ', nrow(RawVCTibble), 
                         'Number of columns: ', ncol(RawVCTibble), collapse = ''))
  }


  ### # Run function build_matrix
  if (pb_log) {
    rvce_log_info(lgr, 'parameter_varCovar_mix99', 
                  'Convert input tibble to list of matrices')
  }
  RawMatrixAsList <- build_matrix(ps_input_tibble = RawVCTibble)
  if (pb_log) {
    rvce_log_info(lgr, 'parameter_varCovar_mix99',
                  paste0('Number of random effects: ', length(RawMatrixAsList)))
  }


  ### # Write original (co-)variances to a file, if specified
  if (! is.null(ps_raw_output_file)) {
    if (pb_log) {
      rvce_log_info(lgr, 'parameter_varCovar_mix99', 'Writing raw (co-)variances to output file ...')
    }
    create_parameter_varCovar_mix99(pl_mat         = RawMatrixAsList,
                                    ps_output_file = ps_raw_output_file)
  }


  ### # Check or Transfrom Matrix if necessary to insure beeing Positive Definit
  if (pb_log) {
    rvce_log_info(lgr, 'parameter_varCovar_mix99',
                  'Checking matrices to be pdf')
  }
  ResultPD <- check_transform_positivedefinit(pl_mat      = RawMatrixAsList,
                                              pn_ratio    = pn_ratio,
                                              pn_eps      = pn_eps, 
                                              pmat_weight = pmat_weight,
                                              pn_digits   = pn_digits,
                                              pb_log      = pb_log,
                                              plogger     = lgr)


  ### # Build Parameter-File in txt-Format with Variances for Mix99
  if (pb_log)
    rvce_log_info(lgr, 'parameter_varCovar_mix99', 'Writing processed matrices to mix99 parameter file ...')

  create_parameter_varCovar_mix99(pl_mat         = ResultPD,
                                  ps_output_file = ps_output_file)
}
