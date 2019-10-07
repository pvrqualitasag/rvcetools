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
#' If you put the paramter psOptionRatio = FALSE, makePD2() is called .
#' For the parameter psOptionRatio = TRUE the function make_pd_rat_ev() is called.
#' The difference is, that with make_pd_rat_ev() you have the option to whish a maximum
#' ratio between largest and smallest eigenvalue.
#'
#' @param psInputFile input csv-file
#' @param psOptionRatio TRUE or FALSE (default) to indicate whether bending should be done based on ratio of largest to smallest eigenvalue
#' @param psRatio maximum ratio between largest and smallest eigenvalue. Only relevant, if psOptionRatio == TRUE
#'
#' @export positivedefinit
positivedefinit <- function(psInputFile,
                            psOptionRatio = FALSE,
                            psRatio       = 100){

  ## # Run function read_vce
  ResultTibble <- read_vce(psInputFile = psInputFile)

  ### # Run function build_matrix
  ResultMatrixAsList <- build_matrix(psInputFile = ResultTibble)

  ### # Check or Transfrom Matrix if necessary to insure beeing Positive Definit
  ResultPD <- check_transform_positivedefinit(psInputFile   = ResultMatrixAsList,
                                              psOptionRatio = psOptionRatio,
                                              psRatio       = psRatio)
  return(ResultPD)

}
