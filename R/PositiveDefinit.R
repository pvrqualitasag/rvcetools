###
###
###
###   Purpose:   Run Bending and Check or Transform Positive Definite Matrix
###   started:   2019/09/30 (skn)
###
### ######################################### ###


#' @title Bending and Check or Transform Positive Definite Matrix
#'
#' @description
#' Read csv-file of the variance component estimates and
#' construct a matrix with this estimates by parts.
#' This matrix is checked if she is positive definite or not.
#' If this matrix is not positive definite, 2 functions may be called.
#' If pn_ratio == NULL, makePD2() is called, otherwise the function make_pd_rat_ev() is called.
#' The difference is, that with make_pd_rat_ev() you have the option to whish a maximum
#' ratio between largest and smallest eigenvalue.
#'
#' @param ps_input_file input csv-file
#' @param pn_ratio maximum ratio between largest and smallest eigenvalue, determines whether ratio method is used or not
#' @param pn_eps lower limit of smallest eigenvalue    
#' @param pmat_weight weight matrix for weighted bending
#' @param pn_digits number of digits to which result will be rounded to
#' @param pb_log indicator whether logs should be produced
#' @param plogger log4r logger object
#'
#' @export positivedefinit
#' @examples 
#' sInputFile <- system.file("extdata","VCE_results.csv", package = "rvcetools")
#' ResultPD <- positivedefinit(ps_input_file = sInputFile)
positivedefinit <- function(ps_input_file,
                            pn_ratio      = NULL,
                            pn_eps        = NULL,    
                            pmat_weight   = NULL,
                            pn_digits     = NULL,
                            pb_log        = FALSE,
                            plogger       = NULL){

  if (pb_log){
    if (is.null(plogger)){
      lgr <- get_rvce_logger(ps_logfile = 'positivedefinit.log', ps_level = 'INFO')
    } else {
      lgr <- plogger
    }
    ### # starting message
    rvce_log_info(lgr, 'positivedefinit',
                  paste0('Started function with parameters: \n * pn_ratio: ', pn_ratio, '\n',
                         ' * pn_eps: ', pn_eps, '\n',
                         ' * pn_digits: ', pn_digits, '\n', collapse = ''))
  }
  
  ## # Run function read_vce
  if (pb_log)
    rvce_log_info(lgr, 'positivedefinit',
                  paste0('Reading input from file: ', ps_input_file, '\n'))
  ResultTibble <- read_vce(ps_input_file = ps_input_file,
                           pb_log      = pb_log,
                           plogger     = lgr)
  if (pb_log)
    rvce_log_info(lgr, 'positivedefinit',
                  paste0('Number of records read: ', nrow(ResultTibble), 
                         ' number of columns: ', ncol(ResultTibble), collapse = ''))

  ### # Run function build_matrix
  if (pb_log)
    rvce_log_info(lgr, 'positivedefinit', 
                  'Convert input tibble to list of matrices')
  ResultMatrixAsList <- build_matrix(psInputFile = ResultTibble)
  if (pb_log)
    rvce_log_info(lgr, 'positivedefinit',
                  paste0('Number of random effects: ', length(ResultMatrixAsList)))

  ### # Check or Transfrom Matrix if necessary to insure beeing Positive Definit
  if (pb_log)
    rvce_log_info(lgr, 'positivedefinit',
                  'Checking matrices for pdf')
  ResultPD <- check_transform_positivedefinit(pl_mat      = ResultMatrixAsList,
                                              pn_ratio    = pn_ratio,
                                              pn_eps      = pn_eps, 
                                              pmat_weight = pmat_weight,
                                              pn_digits   = pn_digits,
                                              pb_log      = pb_log,
                                              plogger     = lgr)
  return(ResultPD)

}
