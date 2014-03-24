#' Calculate statistical characteristics of an event
#' 
#' This function calculates statistical characteristics for detected events.

#' 
#' @param x a time series
#' @param a a scalar specifying starting point of the event
#' @param b a scalar specifying ending point of the event
#' @seealso \code{\link{eventCluster}}
#' @export
#' @examples
#' set.seed(12345)
#' n=128
#' measures(cbfs('box'))
#' measures(cbfs('sine'))


measures <- function(x) {
    x = na.approx(x)
    N <- length(x)
    # Coefficient of variation
    cv = sd((x - min(x, na.rm = TRUE)))/mean((x - min(x, na.rm = TRUE)))
    # non-smoothness
    fs = sd(diff(x, 10))/mean(abs(diff(x, 10)))
    # HD
    hd = abs(mean(x[(round(N/2) + 1):N]) - mean(x[1:round(N/2)]))/(max(x) - min(x, na.rm = TRUE))
    # kurtosis
    k1 <- sum((x - mean(x, na.rm = TRUE))^4, na.rm = TRUE)/(N * var(x, na.rm = TRUE)^2)
    # skewness
    s <- abs(sum((x - mean(x, na.rm = TRUE))^3, na.rm = TRUE)/(N * sd(x, na.rm = TRUE)^3))
    m <- c(cv, fs, hd, k1, s)
    
    # Now do measures on diff data
    diffx <- diff(x, lag = 5)
    # Kurtosis
    k <- sum((diffx - mean(diffx, na.rm = TRUE))^4, na.rm = TRUE)/(N * var(diffx, na.rm = TRUE)^2)
    # scaled max of diff
    fmax = max(diffx)/(max(x) - min(x, na.rm = TRUE))
    # scaled min of diff
    fmin = min(diffx)/(max(x) - min(x, na.rm = TRUE))
    # PP test statistic
    ppstat <- PP.test(x)$statistic
    # ZA test statistic
    zastat <- ur.za.fast(x, model = "trend")$teststat
    
    m <- c(m, k, fmax, fmin, ppstat, zastat)
    
    names(m) <- c("sd", "non-smoothness", "HD", "kurtosis", "skewness", "diff kurtosis", "diff max", "diff min", "PPstat", "ZAstat")
    
    return(m)
} 
