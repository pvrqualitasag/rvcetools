# rvcetools
Tools to work with results from variance components estimation


## Installation
The package can only be installed from `GitHub` using

```
# check whether devtools are available
# if (!'devtools' %in% installed.packages()) install.packages('devtools')
# install the package
devtools::install_github(repo = 'pvrqualitasag/rvcetools')
```

## GitHub Repository
The repository on GitHub is available at: https://github.com/pvrqualitasag/rvcetools


## Background
One of the first steps in the development of a new genetic analysis routine for a new trait is the estimation of 
variance components. This is done using specialised software programs. Most of these programs do not provide a lot 
of functionality related to post-processing results such as comparing results of different estimation runs or 
displaying results graphically. These post-processing tasks are the main goal of this package. 


## Goal
In the current version, analysis results are read from a csv-file. The results can be converted into variance-covariance 
matrices for each random effect. The variance-covariance matrices can be bent using different approaches. 


## Further Information
Some basic information can be obtained from `Getting Started` article. The statements how to create some basic 
plots are shown in the article `plot_vce`. 
