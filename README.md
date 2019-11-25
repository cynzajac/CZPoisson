# CZPoisson
The CZPoisson R package is designed to replicate Catherine Loader's dpois implementation to calculate the approximate Poisson distribution.
For a description of the methods available and some simple examples, please see the
[package vignette](https://github.com/cynzajac/CZPoisson/blob/master/inst/doc/CZPoisson_package.pdf) or the R documentation. 
For installation help, see below.

## Installing the CZPoisson Package
The CZPoisson package can be installed using the devtools package. The following code when executed in R will get you started:
```
install.packages("devtools")
library(devtools)
install_github("cynzajac/CZPoisson")
library(CZPoisson)
```
Note that some versions of devtools do not install all dependencies, so one may need to install those using install.packages() first if there is an error.

You can additionally view the package help or vignette in R:
- `?CZPoisson`
- `vignette("CZPoisson-package")`

[![Build Status](https://travis-ci.org/cynzajac/CZPoisson.svg?branch=master)](https://travis-ci.org/cynzajac/CZPoisson)
[![Codecov test coverage](https://codecov.io/gh/cynzajac/CZPoisson/branch/master/graph/badge.svg)](https://codecov.io/gh/cynzajac/CZPoisson?branch=master)
