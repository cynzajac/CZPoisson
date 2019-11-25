\name{r}
\alias{r}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
\code{r} calculates the probability mass function for the Poisson distribution.
}
\usage{
r(x, lamb, log = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
vector of non-negative counts
}
  \item{lamb}{
%%     ~~Describe \code{lamb} here~~
vector of non-negative means for Poisson distribution
}
  \item{log}{
%%     ~~Describe \code{log} here~~
logical; if TRUE, returns log of probability of x.
If FALSE, returns the probabilities of x
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
\code{r} implements Catherine Loader's algorithm for Poisson probabilities (pmf)
\url{https://lists.gnu.org/archive/html/octave-maintainers/2011-09/pdfK0uKOST642.pdf}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\code{r} returns the (log) probability of the observed count \code{x} given \code{lamb}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Cynthia K. Zajac (^_-)
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
r(6,15)

# log probability
r(1234568,1234567,T)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
