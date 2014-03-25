\name{ur.za.fast}
\alias{ur.za.fast}
\title{Unit root test for events considering a structrual break}
\usage{
ur.za.fast(y, model = c("intercept", "trend", "both"), lag = NULL)
}
\arguments{
  \item{y}{a time series}

  \item{model}{Three choices: `intercept', `trend' or
  `both'}

  \item{lag}{a scalar chosen as lag}
}
\value{
a list consisting of: \item{flag}{0 if the time series is
is stationary; 1 if it is a unit root process}
\item{teststat}{ZA unit root test statistic}
}
\description{
Allowing a structrual break, this function returns flag to
be 0 if the time series is is stationary and 1 if it is a
unit root process. This function is written refering to the
\code{ur.za} function in the \code{urza} package, but it
speeds up executation using the linear regression function
in the \code{RcppArmadillo} package.
}
\examples{
x=cbfs_red('box')
ur.za.fast(x,'both')
x=cbfs_red('cr')
ur.za.fast(x,'both')
}
\references{
Eric Zivot and Donald W K Andrews (1992). Further evidence
on the great crash, the oil-price shock, and the unit-root
hypothesis. \emph{Journal of Business & Economic
Statistics}, \bold{20}(1), 25-44.
\url{http://dx.doi.org/10.1198/073500102753410372}.
}
\seealso{
\code{\link{noiseTests}}
}

