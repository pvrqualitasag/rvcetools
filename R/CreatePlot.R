#' @title Reading VCE results for graphics
#'
#' @import ggplot2
#'
#' @export read_vce4grafics
read_vce4grafics <- function(psInputFile,
                     pbLog = FALSE){

  # Check that files exist
  if (!file.exists(psInputFile))
    stop("Cannot find input file: ",psInputFile)

  # Read all VCE results
  d.vr <- read.table(file = psInputFile, sep =";", header = TRUE)

  orderTraitNames <- function(traits) {
    traits <- as.character(traits)
    trtvec <- strsplit(traits, split = "+", fixed = TRUE)[[1]]
    paste(trtvec[order(trtvec)], collapse = "+")
  }
  d.vr$traits <- as.factor(apply(d.vr[,"traits",drop=FALSE], 1, orderTraitNames))
  d.vr$trait_combination <- as.factor(apply(d.vr[,"trait_combination",drop=FALSE], 1, orderTraitNames))
  d.vr$estimate <- as.numeric(as.character(d.vr$estimate))
  d.vr$STD_ERR_estimate <- as.numeric(as.character(d.vr$STD_ERR_estimate))

  # Resulting dataframe
  return(d.vr)

}




#' @title Create Plot to Overview VCE results for genetic correlation
#'
#' @export plot_gencorr
plot_gencorr <- function(psInputFile){

  t.corr <- psInputFile[psInputFile$type=="genetic_correlation",]
  gg <- ggplot(data = t.corr, aes(traits, estimate)) +
    geom_point(aes(colour = data_subset_number)) +
    theme(axis.text.x = element_text(angle = 90)) +
    facet_grid(rows= vars(model_name)) +
    ggtitle("Genetic correlation")

  return(gg)

}



#' @title Create Plot to Overview VCE results for heritability
#'
#' @export plot_h2
plot_h2 <- function(psInputFile){

  t.h2 <- psInputFile[psInputFile$type=="heritability",]
  gg <- ggplot(data = t.h2, aes(traits, estimate)) +
    geom_jitter(aes(colour = data_subset_number), width = 0.15) +
    facet_grid(rows= vars(model_name)) +
    ggtitle("Heritability")

  return(print(gg))

}



#' @title Create Plot to Overview VCE results for variances
#'
#' @export plot_var
plot_var <- function(psInputFile){

  t.variance <- psInputFile[psInputFile$type=="variance",]

  gg <- ggplot(data = t.variance, aes(traits, estimate)) +
    geom_point(aes(colour = factor(data_subset_number))) +
    facet_grid(rows= vars(model_name), cols = vars(random_effect)) +
    ggtitle("Variance")

  return(print(gg))

}
