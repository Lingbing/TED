\name{eventDetection}
\alias{eventDetection}
\title{Detect events in time series}
\usage{
eventDetection(x, w, noiseType = c("white", "red"), parallel = FALSE, alpha,
  data = c("art", "real"))
}
\arguments{
  \item{x}{a time series}

  \item{w}{size of the sliding window}

  \item{noiseType}{background noise assumed for x. There
  are two options: white noise or red noise}

  \item{parallel}{logical, if TRUE then codes are executed
  in parallel using \code{foreach} package. The user must
  register a parallel backend to use by the \code{doMC}
  package}

  \item{alpha}{the significance level. When the noise test
  p value of the subsequence is smaller than this
  significance level, it is a potential event.}

  \item{data}{type of data being analysed. There are two
  options: 'art' if analysed data is artificial data and
  'real' if analysed data is real world turbulence data.}
}
\value{
an object of class \code{events} with the components listed
below:

\item{x}{the original time series}

\item{start}{a vector consisting of starting points of
events}

\item{end}{a vector consisting of ending points of events}

\item{nevents}{number of detected events}
}
\description{
This function find events from a time series.
}
\examples{
##################################
#   1st art eg (white noise)
##################################
set.seed(123)
n=128
types=c('box','rc','cr','sine')
shapes=matrix(NA,20,n)
for (i in 1:20){
  shapes[i,]=cbfs(type=types[sample(1:4,1)])
}
whitenoise=ts2mat(rnorm(128*20),128)
x=c(rnorm(128),t(cbind(shapes,whitenoise)))
plot(x,ty='l')
w=128
alpha=0.05
events=eventDetection(x,w,'white',parallel=TRUE,alpha,'art')
##################################
#   2nd art eg (red noise)
##################################
set.seed(123)
coeff=0.5;s=1
x=c(arima.sim(list(order = c(1,0,0),ar=coeff),n=500,sd=s),
    cbfs_red("rc"),arima.sim(list(order = c(1,0,0),ar=coeff),n=400,sd=s),
    cbfs_red("cr"),arima.sim(list(order = c(1,0,0),ar=coeff),n=400,sd=s),
    cbfs_red("box"),arima.sim(list(order = c(1,0,0),ar=coeff),n=400,sd=s),
    cbfs_red("sine"),arima.sim(list(order = c(1,0,0),ar=coeff),n=1000,sd=s),
    arima.sim(list(order = c(1,0,0),ar=0.8),n=1100,sd=4))
w=128; alpha=0.05
events=eventDetection(x,w,'red',parallel=TRUE,alpha,'art')
}
\references{
Yanfei Kang, Danijel Belusic, Kate Smith-Miles (2014):
Detecting and Classifying Events in Noisy Time Series.
\emph{J. Atmos. Sci.}, \bold{71}, 1090-1104.
\url{http://dx.doi.org/10.1175/JAS-D-13-0182.1}.
}
\seealso{
\code{\link{noiseTests}}, \code{\link{eventExtraction}},
\code{\link{plot.events}}
}

