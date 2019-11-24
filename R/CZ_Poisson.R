##The Poisson parameter Lambda (λ) is the total number of events (k)
##divided by the number of units (n) in the data (λ = k/n)

## Loader's algorithm ###
delta <- function(n){
  1/(12*n) - 1/(360*n^3) + 1/(1260*n^5)
}

D0 <- function(e){ 
  e*log(e) + 1 - e
}

r_single <- function(x,lamb, log = FALSE){
  if (log == FALSE){
    return(r_single_nolog(x,lamb))
  }
  else{
    return(r_single_log(x,lamb))
  }
}
  
r_single_nolog <- function(x,lamb){ ##user needs to provide x and lamb
  if (x != round(x)){
    return(0)
  }
  if (lamb == 0 && x == 0){
    return(1)
  }
  else if (lamb == 0 && x > 0) {
    return(0)
  }
  if (lamb < 0){
    return(NaN)
  }
  if (x < 0){
    return(0)
  }
  if (x > 5) {
    (1/sqrt(2*pi*x))*exp(-delta(x)-lamb*D0(x/lamb))
  } 
  else if (x == 0) {
    exp(-lamb)
  }
  else if (x <= 5){
    (lamb^x/factorial(x)*exp(-lamb))
  }
  else {
    return(0)
  }
}

r_single_log <- function(x,lamb){
  if (x != round(x)){
    return(-Inf)
  }
  if (lamb == 0 && x == 0){
    return(0)
  }
  else if (lamb == 0 && x > 0) {
    return(-Inf)
  }
  if (lamb < 0){
    return(NaN)
  }
  if (x < 0){
    return(-Inf)
  }
  if (x > 5) {
    -0.5*log(2*pi*x)- delta(x)-lamb*D0(x/lamb)
  } 
  else if (x == 0) {
    -lamb
  }
  else if (x <= 5){
    (lamb^x/factorial(x)*exp(-lamb))
    x*log(lamb) - lfactorial(x) - lamb
  }
  else {
    return(-Inf)
  }
}

r <- Vectorize(r_single)

###################################################
cdf_single <- function(x, lamb, lower.tail = TRUE, log.p = FALSE){
  i = floor(x)
  if (i < 0 && lower.tail == TRUE && log.p == FALSE){
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
    return(NaN)
  }
  if (lower.tail == TRUE ){
    d = FALSE
  }
  else {
    d = TRUE
  }
  pgamma(q = lamb, shape = i + 1, rate = 1, lower.tail = d, log.p = log.p)
}

cdf <- Vectorize(cdf_single)

