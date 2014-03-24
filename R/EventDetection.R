#' Detect events in time series
#' 
#' This function find events from a  time series.
#' 
#' @param x a time series
#' @param w size of the sliding window
#' @param noiseType background noise assumed for x. There are two options: white noise or red noise
#' @param parallel logical, if TRUE then codes are executed in parallel using \code{foreach} package. The user must register a parallel backend
#'  to use by the \code{doMC} package
#' @param alpha the significance level. When the noise test p value of the subsequence is smaller than this significance level,
#' it is a potential event.
#' @param data type of data being analysed. There are two options: 'art' if analysed data is artificial data and 'real' if 
#' analysed data is real world turbulence data.
#' @seealso \code{\link{noiseTests}}, \code{\link{eventExtraction}}, \code{\link{plot.events}}
#' @references Yanfei Kang, Danijel Belusic, Kate Smith-Miles (2014): Detecting and Classifying Events in Noisy Time 
#' Series. \emph{J. Atmos. Sci.}, \bold{71}, 1090-1104.
#' \url{http://dx.doi.org/10.1175/JAS-D-13-0182.1}.
#' @export
#' @examples
#' ##################################
#' #   1st art eg (white noise)
#' ##################################
#' set.seed(12345)
#' n=128
#' types=c('box','rc','cr','sine')
#' shapes=matrix(NA,20,n)
#' for (i in 1:20){
#'   shapes[i,]=cbfs(type=types[sample(1:4,1)])
#' }
#' whitenoise=ts2mat(rnorm(128*20),128)
#' x=c(rnorm(128),t(cbind(shapes,whitenoise)))
#' plot(x,ty='l')
#' w=128
#' alpha=0.05
#' events=eventDetection(x,w,alpha,'art')
#' ##################################
#' #   2nd art eg (red noise)
#' ##################################
#' set.seed(12345)
#' coeff=0.5;s=1
#' x=c(arima.sim(list(order = c(1,0,0),ar=coeff),n=500,sd=s),
#'     cbfs_red("rc"),arima.sim(list(order = c(1,0,0),ar=coeff),n=400,sd=s),
#'     cbfs_red("cr"),arima.sim(list(order = c(1,0,0),ar=coeff),n=400,sd=s),
#'     cbfs_red("box"),arima.sim(list(order = c(1,0,0),ar=coeff),n=400,sd=s),
#'     cbfs_red("sine"),arima.sim(list(order = c(1,0,0),ar=coeff),n=1000,sd=s),
#'     arima.sim(list(order = c(1,0,0),ar=0.8),n=1100,sd=4))
#' w=128; alpha=0.05
#' events=eventDetection(x,w,'red',parallel=TRUE,alpha,'art')

eventDetection <- function(x, w,noiseType = c("white", "red"),parallel=FALSE, alpha, data = c("art", "real")) {
    noiseType <- match.arg(noiseType)
    tests = noiseTests(x, w, noiseType = noiseType, parallel=parallel)
    events = eventExtraction(tests, w, alpha)
    nevents = events$nevents
    data = match.arg(data)
    if (data == "art") {
        start = round((events$start + events$end)/2)
        end = start + w - 1
    }
    if (data == "real") {
        start = events$start + round(w/4)
        end = events$end + w - 1 - round(w/4)
    }
    cat(length(start), "events found.")
    results <- list(start = start, end = end, nevents = length(start))
    return(results)
} 
