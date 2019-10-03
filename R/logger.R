### #
### #
### #
### #   Purpose:   Functions Related to Logging
### #   started:   2019-10-03 (pvr)
### #
### # ############################################## ###

#' @title Create log4r Logger for package
#'
#' @param ps_logfile name of the logfile
#' @param ps_level logger level
#'
#' @return rvcetools_logger
#' @export get_rvce_logger
#' 
#' @examples
#' rvce_logger <- get_rvce_logger()
get_rvce_logger <- function(ps_logfile = 'rvcetools.log', ps_level = 'FATAL'){
  rvcetools_logger <- log4r::create.logger(logfile = ps_logfile, level = ps_level)
  return(rvcetools_logger)
}


#' @title Wrapper for log4r info 
#'
#' @param plogger log4r logger object
#' @param ps_msg logging message
#'
#' @export rvce_log_info
#'
#' @examples
#' rvce_logger <- get_rvce_logger()
#' rvce_log_level(rvce_logger, 'INFO')
#' rvce_log_info(rvce_logger)
rvce_log_info <- function(plogger, ps_caller, ps_msg){
  s_msg <- paste0(ps_caller, ' -- ', ps_msg, collapse = '')
  log4r::info(logger = plogger, message = s_msg)
}


#' @title Wrapper for log4r debug 
#'
#' @param plogger log4r logger object
#' @param ps_msg logging message
#'
#' @export rvce_log_debug
#'
#' @examples
#' rvce_logger <- get_rvce_logger()
#' rvce_log_level(rvce_logger, 'DEBUG')
#' rvce_log_debug(rvce_logger)
rvce_log_debug <- function(plogger, ps_caller, ps_msg){
  s_msg <- paste0(ps_caller, ' -- ', ps_msg, collapse = '')
  log4r::debug(logger = plogger, message = s_msg)
}


#' @title Wrapper for log4r warn 
#'
#' @param plogger log4r logger object
#' @param ps_msg logging message
#'
#' @export rvce_log_warn
#'
#' @examples
#' rvce_logger <- get_rvce_logger()
#' rvce_log_level(rvce_logger, 'WARN')
#' rvce_log_warn(rvce_logger)
rvce_log_warn <- function(plogger, ps_caller, ps_msg){
  s_msg <- paste0(ps_caller, ' -- ', ps_msg, collapse = '')
  log4r::warn(logger = plogger, message = s_msg)
}


#' @title Wrapper for log4r error 
#'
#' @param plogger log4r logger object
#' @param ps_msg logging message
#'
#' @export rvce_log_error
#'
#' @examples
#' rvce_logger <- get_rvce_logger()
#' rvce_log_level(rvce_logger, 'ERROR')
#' rvce_log_error(rvce_logger)
rvce_log_error <- function(plogger, ps_caller, ps_msg){
  s_msg <- paste0(ps_caller, ' -- ', ps_msg, collapse = '')
  log4r::error(logger = plogger, message = s_msg)
}


#' @title Wrapper for log4r fatal 
#'
#' @param plogger log4r logger object
#' @param ps_msg logging message
#'
#' @export rvce_log_fatal
#'
#' @examples
#' rvce_logger <- get_rvce_logger()
#' rvce_log_level(rvce_logger, 'FATAL')
#' rvce_log_fatal(rvce_logger)
rvce_log_fatal <- function(plogger, ps_caller, ps_msg){
  s_msg <- paste0(ps_caller, ' -- ', ps_msg, collapse = '')
  log4r::fatal(logger = plogger, message = s_msg)
}


#' @title Wrapper to set the level of a logger
#'
#' @param plogger log4r logger object
#' @param ps_level new level of plogger
#'
#' @export rvce_log_level
#'
#' @examples
#' rvce_logger <- get_rvce_logger()
#' rvce_log_level(rvce_logger, 'INFO')
rvce_log_level <- function(plogger, ps_level = c('DEBUG', 'INFO', 'WARN', 'ERROR', 'FATAL')){
  if (!missing(ps_level) & length(ps_level) > 1) stop(" *** ERROR in level(): only one 'level' allowed.")
  ps_level <- match.arg(ps_level)
  log4r::level(plogger) <- ps_level
}
