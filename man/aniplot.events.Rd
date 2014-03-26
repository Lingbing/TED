\name{aniplot.events}
\alias{aniplot.events}
\title{Generate a gif for the event detection process}
\usage{
aniplot.events(x, w, noiseType = c("white", "red"), alpha = 0.05,
  main = "Animation plot of events", xlab = "t", ylab = "x",
  movie.name = "animation.gif", interval = 0.05, ani.width = 1000,
  ani.height = 400, outdir = getwd())
}
\arguments{
  \item{x}{a time series}

  \item{w}{a scalar specifying the size of the sliding
  window}

  \item{noiseType}{background noise assumed for x. There
  are two options: white noise or red noise}

  \item{alpha}{the significance level. When the noise test
  p value of the subsequence is smaller than this
  significance level, it is a potential event.}

  \item{main}{title of the animiation plot; default is
  `Animation plot of event detection'.}

  \item{xlab}{x label of the animation plot; default is
  `t'.}

  \item{ylab}{y label of the animation plot; default is
  `x'.}

  \item{movie.name}{name of the output gif file; default is
  `animation.gif'.}

  \item{interval}{a positive number to set the time
  interval of the animation (unit in seconds); default is
  0.05.}

  \item{ani.width}{width of the gif file (unit in px),
  default is 1000.}

  \item{ani.height}{height of the gif file (unit in px);
  default is 400.}

  \item{outdir}{character: specify the output directory
  when exporting the animations; default to be the current
  working directory.}
}
\value{
...
}
\description{
This function generates a gif file demonstrating how the
event detection process is implemented.
}
\examples{
set.seed(12345)
# generate an artificial time series
x=c(rnorm(128),cbfs(type="box"),rnorm(128),cbfs(type="rc"),rnorm(128))
# generate a gif file to show the event detection process
aniplot.events(x,w=128,noiseType="white",outdir=getwd())
}
\references{
Yihui Xie (2013). animation: An R Package for Creating
Animations and Demonstrating Statistical Methods.
\emph{Journal of Statistical Software}, \bold{53}(1), 1-27.
\url{http://www.jstatsoft.org/v53/ i01/}.
}
\seealso{
\code{\link{noiseTests}}, \code{\link{eventExtraction}},
\code{\link{plot.events}}
}

