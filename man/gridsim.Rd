\name{gridsim}
\alias{gridsim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gridsim(nrow, ncol, density, time)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nrow}{
%%     ~~Describe \code{nrow} here~~
}
  \item{ncol}{
%%     ~~Describe \code{ncol} here~~
}
  \item{density}{
%%     ~~Describe \code{density} here~~
}
  \item{time}{
%%     ~~Describe \code{time} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (nrow, ncol, density, time) 
{
    griddf = gridseed(r = nrow, j = ncol, p = density)
    origin = griddf
    for (i in 1:time) griddf = gridmove(infodf = griddf, t = i, 
        r = nrow, j = ncol)
    cargrid = list(griddf = griddf, dimension = c(nrow, ncol), 
        density = density, origin = origin)
    class(cargrid) = "CarGrid"
    cargrid
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
