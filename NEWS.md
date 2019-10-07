# rvcetools 0.0.3

* Added feature of rounding components of variance-covariance matrices. The rounding is done inside of the different bending function. After the bending procedure which changes the eigenvalues, the resulting matrix is reconstructed. The components of this re-constructed result matrix are rounded just before it is returned. The eigenvalues of the rounded result matrix are checked and if one is negative, the program stops with an error. For all matrices that do not require bending the rounding is applied on the original matrices.
* Corrected bug of loosing row- and column-names after bending functions.

# rvcetools 0.0.2

* Removed un-needed support for Rcpp

# rvcetools 0.0.1

* Initial version