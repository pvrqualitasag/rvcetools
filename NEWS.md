# rvcetools 0.0.5

* Implemented bending method described in [Jorjani et al.  2003](https://www.journalofdairyscience.org/article/S0022-0302(03)73646-7/fulltext)


# rvcetools 0.0.4

* Fixed bugs with rounding. Removed formatted output with restricting the number of digits from the output. The only formating that is done in the output is setting the scientific format to `FALSE`. 
* Suppressed the warnings caused by splitting the input into two traits where in some records only one trait was present.

# rvcetools 0.0.3

* Added feature of rounding components of variance-covariance matrices. The rounding is done inside of the different bending function. After the bending procedure which changes the eigenvalues, the resulting matrix is reconstructed. The components of this re-constructed result matrix are rounded just before it is returned. The eigenvalues of the rounded result matrix are checked and if one is negative, the program stops with an error. For all matrices that do not require bending the rounding is applied on the original matrices.
* Corrected bug of loosing row- and column-names after bending functions.

# rvcetools 0.0.2

* Removed un-needed support for Rcpp

# rvcetools 0.0.1

* Initial version