\name{cdf}
\alias{cdf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
cdf
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
\code{cdf} calculates the cumulative probability function for the Poisson distribution.
}
\usage{
cdf(x, lamb, lower.tail = TRUE, log.p = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
vector of quantiles
}
  \item{lamb}{
%%     ~~Describe \code{lamb} here~~
vector of non-negative means for Poisson distribution
}
  \item{lower.tail}{
%%     ~~Describe \code{lower.tail} here~~
logical; if TRUE, computes the probability of observing count less than or equal to x. If FALSE, computes the probability of observing a count greater than x
}
  \item{log.p}{
%%     ~~Describe \code{log.p} here~~
logical; if TRUE, returns log of cumulative probability of x.
If FALSE, returns the cumulative probabilities of x
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
\code{cdf} uses the cumulative probability function of gamma distribution
\deqn{
1/\Gamma(floor(x)+1) * \gamma(floor(x)+1,\lambda)
}

%%
%%\rfloor

}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Cynthia K. Zajac (-_^)
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
cdf(123456789, 123456789)

# Upper tail probability
cdf(5, 5:10, lower.tail = FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
