#' @title Reading VCE results
#'
#' @description
#' First attempt to read variance component estimation (VCE) from a csv-file
#' and storing the input in tibble
#'
#' @export read_vce
read_vce <- function(psInputFile,
                     pbLog = FALSE){

  ### # Read all VCE results
  tbl_vce <- readr::read_delim(file = psInputFile, delim = ";")
  # Transform "---" to 0 coming from VCE software some times
  tbl_vce$estimate[tbl_vce$estimate == "---"] <- "0"
  # Transform estimates to numeric
  tbl_vce$estimate <- as.numeric(as.character(tbl_vce$estimate))

  ### # Resulting tibble
  return(tbl_vce)

}
