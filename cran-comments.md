## Update

This is an update from handwriter 3.2.4 to 3.3.0. It:

* Fixes a bug introduced in 3.2.3 that lowered the accuracy of the closed-set writership model.
* Fixes the current CRAN check ERRORs on r-devel-linux-x86_64-debian-clang and r-devel-linux-x86_64-debian-gcc ("Package required but not available: 'rjags'"). rjags fails to install on those machines because they have JAGS 5.0.1 and rjags 4-18 requires JAGS 4. rjags is now in Suggests. It is only used by `fit_model()`, which checks for rjags with `requireNamespace()` and gives an informative error if it isn't available. The tests that need rjags are skipped when it isn't available.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

We checked 2 reverse dependencies, handwriterApp 2.0.0 and handwriterRF 1.1.1, with R CMD check --as-cran against handwriter 3.3.0.

 * We saw 0 new problems
 * We failed to check 0 packages
