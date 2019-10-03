#' @title Reading VCE Results From CSV File 
#'
#' @description
#' First attempt to read variance component estimation (VCE) results 
#' from a csv-file and storing them in tibble.
#'
#' @param psInputFile name of the input file in csv-format
#' @param pbLog flag indicating whether logging should be done
#' @param plogger logger object from outside
#' 
#' @export read_vce
#' 
#' @examples 
#' # specify input file
#' sInputFile <- system.file("extdata","VCE_results.csv", package = "rvcetools")
#' tbl_vce <- read_vce(psInputFile = sInputFile)
read_vce <- function(psInputFile,
                     pbLog   = FALSE,
                     plogger = NULL){

  if (pbLog) {
    if (is.null(plogger)){
      lgr <- get_rvce_logger(ps_logfile = 'read_vce.log', ps_level = 'INFO')
    } else {
      lgr <- plogger
    }
  }
  ### # Read all VCE results
  if (pbLog) 
    rvce_log_info(plogger   = lgr,
                  ps_caller = 'read_vce', 
                  ps_msg    = paste0('Reading from input file: ', psInputFile))
  tbl_vce <- readr::read_delim(file = psInputFile, delim = ";")
  if (pbLog) 
    rvce_log_info(plogger   = lgr,
                  ps_caller = 'read_vce', 
                  ps_msg    = paste0('Number of records read: ', nrow(tbl_vce)))
  # Transform "---" to 0 coming from VCE software some times
  tbl_vce$estimate[tbl_vce$estimate == "---"] <- "0"
  if (pbLog) 
    rvce_log_info(plogger   = lgr,
                  ps_caller = 'read_vce', 
                  ps_msg    = 'Missing estimates replaced with 0')
  # Transform estimates to numeric
  tbl_vce$estimate <- as.numeric(as.character(tbl_vce$estimate))  
  if (pbLog) 
    rvce_log_info(plogger   = lgr,
                  ps_caller = 'read_vce', 
                  ps_msg    = 'Column of estimates converted to numeric values')

  ### # Resulting tibble
  return(tbl_vce)

}
