## Loader's algorithm ##

delta <- function(n){
  ## Delta is a helper function to calculate Poisson distribution
  1/(12*n) - 1/(360*n^3) + 1/(1260*n^5)
}

D0 <- function(e){
  ## Deviance 0 is a helper function to calculate Poisson distribution
  e*log(e) + 1 - e
}

r_single <- function(x,lamb, log = FALSE){
  ## r_single chooses the proper action for the log Poisson distribution
  if (log == FALSE){
    return(r_single_nolog(x,lamb))
  }
  else{
    return(r_single_log(x,lamb))
  }
}

r_single_nolog <- function(x,lamb){
  ## r_single_nolog calculates for the no log Poisson distribution
  if (x != round(x)){
    # checking for integer input
    return(0)
  }
  if (lamb == 0 && x == 0){
    # all point mass at x = 0
    return(1)
  }
  else if (lamb == 0 && x > 0) {
    # no point mass at x > 0
    return(0)
  }
  if (lamb < 0){
    # Invalid input
    return(NaN)
  }
  if (x < 0){
    # x is non-negative integer
    return(0)
  }
  if (x > 5) {
    # Calculate the approximate poisson distribution (Catherine Loader's)
    (1/sqrt(2*pi*x))*exp(-delta(x)-lamb*D0(x/lamb))
  }
  else if (x == 0) {
    # Calculate the exact poisson distribution
    exp(-lamb)
  }
  else if (x <= 5){
    # Calculate the exact poisson distribution
    (lamb^x/factorial(x)*exp(-lamb))
  }
}

r_single_log <- function(x,lamb){
  ## r_single_log calculates the log for the Poisson distribution
  if (x != round(x)){
     # checking for integer input
    return(-Inf)
  }
  if (lamb == 0 && x == 0){
    # all point mass at x = 0
    return(0)
  }
  else if (lamb == 0 && x > 0) {
    # no point mass at x > 0
    return(-Inf)
  }
  if (lamb < 0){
    # Invalid input
    return(NaN)
  }
  if (x < 0){
    # x is non-negative integer
    return(-Inf)
  }
  if (x > 5) {
    # Calculate the approximate log poisson distribution (Catherine Loader's)
    -0.5*log(2*pi*x)- delta(x)-lamb*D0(x/lamb)
  }
  else if (x == 0) {
    # Calculate the exact log poisson distribution
    -lamb
  }
  else if (x <= 5){
    # Calculate the exact log poisson distribution
    x*log(lamb) - lfactorial(x) - lamb
  }
}

r <- Vectorize(r_single)
## Function needs vectorization because of all
## if statements in the function (r_single)


## Cumulative distribution function (cdf) ##

cdf_single <- function(x, lamb, lower.tail = TRUE, log.p = FALSE){
  # Calculates the cdf of the poisson distribution
  i = floor(x)
  # Obtain the next lowest integer from x
  if (i < 0 && lower.tail == TRUE && log.p == FALSE){
    # No probability for x < 0
    return(0)
  }
  else if (i < 0 && lower.tail == TRUE && log.p == TRUE ){
    return(-Inf)
  }
  else if (i < 0 && lower.tail == FALSE && log.p == FALSE){
    return(1)
  }
  else if (i < 0 && lower.tail == FALSE && log.p == TRUE ){
    return(0)
  }
  if (lamb < 0){
    # Invalid input
    return(NaN)
  }
  if (lower.tail == TRUE ){
    # Flip the tail for the gamma
    d = FALSE
  }
  else {
    d = TRUE
  }
  # Calculate the approximation based on the gamma cdf
  pgamma(q = lamb, shape = i + 1, rate = 1, lower.tail = d, log.p = log.p)
}

cdf <- Vectorize(cdf_single)
## Function needs vectorization because of all
## if statements in the function (cdf_single)

