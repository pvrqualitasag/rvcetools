# rvcetools 0.0.3

* Added feature of rounding components of variance-covariance matrices. The rounding is done inside of the bending function. The bending is done before the result is returned. For all matrices that do not require bending the rounding is applied on the original matrices.
* Corrected bug of loosing row- and column-names after bending functions.

# rvcetools 0.0.2

* Removed un-needed support for Rcpp

# rvcetools 0.0.1

* Initial version