#' Perform noise tests for a time series 
#' 
#' This function performs noise tests on the sliding subsequences extracted from a  time series. Choose the background noise
#' type via \code{noiseType} according to the application context. In atmospheric turbulence, red noise is used.
#' We first use the Phillips-Perron (PP) Unit Root Test to test for the unit root process.
#' For the stationary processes, red noise tests are performed to test for events. For those cases tested to be unit root processes,
#' we have to take into consideration a special situation when there is a structural break in the process. The reason comes from the difficulty for PP test
#' to distinguish random walk processes from a stationary process contaminated by a structural break, both of which 
#' result in non-rejection of the null hypothesis. Random-walk processes are not considered as events since they are known to be brownian noise, 
#' but stationary processes with structure breaks are, so it is essential to distinguish them. To this end, an additional test called Zivot & Andrews (ZA)
#' unit root test is introduced.This test allows for a structural break in either the intercept or in the slope of the
#' trend function of the underlying series. Rejection of the null hypothesis indicates a potential event (stationary process with a structural break).
#' Random walk processes result in non-rejection of the null hypothesis.

#' 
#' @param x a time series
#' @param w a scalar specifying the size of the sliding window
#' @param noiseType background noise assumed for x. There are two options: white noise or red noise
#' @param parallel logical, if TRUE then codes are executed in parallel using the \code{foreach} package. The user must register a parallel backend
#'  to use by the \code{doMC} package 
#' @seealso \code{\link{eventExtraction}}, \code{\link{plot.events}}
#' @references Pierre Perron (1998). Trends and random walks in macroeconomic time series: Further evidence from a new approach. 
#' \emph{Journal of economic dynamics and control}, \bold{12}(2), 297-332. \url{http://dx.doi.org/10.1016/0304-3932(82)90012-5}.
#' @references Eric Zivot and Donald W K Andrews (1992). Further evidence on the great crash, 
#' the oil-price shock, and the unit-root hypothesis. \emph{Journal of Business & Economic Statistics}, \bold{20}(1), 25-44. \url{http://dx.doi.org/10.1198/073500102753410372}.
#' @references Yanfei Kang, Danijel Belusic and Kate Smith-Miles (2014). Detecting and Classifying Events in Noisy Time Series.
#'  \emph{J. Atmos. Sci.}, \bold{71}, 1090-1104.
#' \url{http://dx.doi.org/10.1175/JAS-D-13-0182.1}.
#' @export
#' @examples
#' set.seed(12345)
#' n=128
#' types=c('box','rc','cr','sine')
#' shapes=matrix(NA,20,n)
#' for (i in 1:20){
#'   shapes[i,]=cbfs(type=types[sample(1:4,1)])
#' }
#' whitenoise=ts2mat(rnorm(128*20),128)
#' x=c(t(cbind(shapes,whitenoise)))
#' plot(x,ty='l')
#' w=128
#' # execute loops sequentially
#' tests=noiseTests(x,w,'white',parallel=FALSE)
#' # execute loops in parallel
#' # register a parallel backend
#' library(doMC);library(foreach)
#' registerDoMC(cores=8)
#' tests=noiseTests(x,w,'white',parallel=TRUE)
noiseTests <- function(x, w, noiseType = c("white", "red"),parallel=FALSE) {
    noiseType <- match.arg(noiseType)
    x = as.numeric(x)
    lx = length(x)
    N = length(x) - w + 1
    l = round(log(w))
    if (parallel){
      registerDoMC(cores = 8)
    if (noiseType == "white") {
        testsPvalues = foreach(i = 1:N, .combine = c) %dopar% {
            xx = x[i:(i + w - 1)]
            Box.test(xx, type = "Ljung-Box", lag = l)$p.value
        }
        
    } else if (noiseType == "red") {
        testsPvalues = foreach(i = 1:N, .combine = c) %dopar% {
            xx = x[i:(i + w - 1)]
            if (sum(is.na(xx)) > w/4) {
                NA
            } else {
                xx = na.approx(xx)
                if (PP.test(xx)$p.value > 0.05) {
                  ur.za.fast(xx, "both")$flag
                } else {
                  xx = detrendc(xx)
                  model = ar(xx, order.max = 1, method = "ols")
                  Box.test(model$resid, lag = l, type = "Ljung-Box", fitdf = 1)$p.value
                }
            }
        }
    } else {
        stop("Not a correct noise type is selected ('white' or 'red') ")
    }}else{
      pbar <- txtProgressBar(min = 0, max = N, style = 3)
      if (noiseType == "white") {
        testsPvalues = foreach(i = 1:N, .combine = c) %do% {
          setTxtProgressBar(pbar, i)
          xx = x[i:(i + w - 1)]
          Box.test(xx, type = "Ljung-Box", lag = l)$p.value
        }
        
      } else if (noiseType == "red") {
        testsPvalues = foreach(i = 1:N, .combine = c) %do% {
          setTxtProgressBar(pbar, i)
          xx = x[i:(i + w - 1)]
          if (sum(is.na(xx)) > w/4) {
            NA
          } else {
            xx = na.approx(xx)
            if (PP.test(xx)$p.value > 0.05) {
              ur.za.fast(xx, "both")$flag
            } else {
              xx = detrendc(xx)
              model = ar(xx, order.max = 1, method = "ols")
              Box.test(model$resid, lag = l, type = "Ljung-Box", fitdf = 1)$p.value
            }
          }
        }
      } else {
        stop("Not a correct noise type is selected ('white' or 'red') ")
      }}
    
    return(testsPvalues)
    
} 
