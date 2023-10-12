## R CMD check results

0 errors | 0 warnings | 1 note

* checking C++ specification ... NOTE
    Specified C++11: please drop specification unless essential

With R 3.4.0, and RcppArmadillo 0.7.960.*, we turn C++11 on as OpenMP support within Armadillo prefers / requires it.

## revdepcheck results

We checked 0 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages