#' Perform noise tests for a time series 
#' 
#' This function performs noise tests on the sliding subsequences extracted from a  time series.
#' 
#' @param x a time series
#' @param w a scalar specifying the size of the sliding window
#' @param noiseType background noise assumed for x. There are two options: white noise or red noise
#' @param parallel logical, if TRUE then codes are executed in parallel using \code{foreach} package. The user must register a parallel backend
#'  to use by the \code{doMC} package 
#' @keywords time series, noise tests
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
    registerDoMC(cores = 8)
    noiseType <- match.arg(noiseType)
    x = as.numeric(x)
    lx = length(x)
    N = length(x) - w + 1
    l = round(log(w))
    if (parallel){
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
                  za_test = ur.za.fast(xx, "both")
                  ifelse(za_test$teststat < za_test$cval[2], 0, 1)
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
              za_test = ur.za.fast(xx, "both")
              ifelse(za_test$teststat < za_test$cval[2], 0, 1)
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
