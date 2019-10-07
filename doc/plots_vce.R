## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- include=FALSE------------------------------------------------------
psInputFile <- system.file("extdata","VCE_results.csv", package = "rvcetools")

## ---- include=FALSE, message=FALSE---------------------------------------
# Run function read_vce
ResultDF <- rvcetools::read_vce4grafics(psInputFile = psInputFile)

## ---- echo=FALSE, out.width=720------------------------------------------
### # Plot heritability
rvcetools::plot_h2(psInputFile = ResultDF)

## ---- echo=FALSE, out.width=720------------------------------------------
# Plot genetic correlations
rvcetools::plot_gencorr(psInputFile = ResultDF)

## ---- echo=FALSE, out.width=720------------------------------------------
rvcetools::plot_var(psInputFile = ResultDF)

## ------------------------------------------------------------------------
sessionInfo()

