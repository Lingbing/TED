#' Unit root test for events considering a structrual break
#' 
#' This function performs unit root test for events considering a structrual break.

#' 
#' @param y a time series
#' @param model Three choices: 'intercept', 'trend' or 'both')
#' @param lag a scalar chosen as lag
#' @export

ur.za.fast <- function(y, model = c("intercept", "trend", "both"), lag = NULL) {
    n <- length(y)
    model <- match.arg(model)
    if (is.null(lag)) 
        lag <- 0
    lag <- as.integer(lag)
    datmat <- matrix(NA, n, lag + 4)
    idx <- 2:(n - 2)
    trend <- seq(1, n)
    datmat[, 1:4] <- cbind(y, 1, c(NA, y)[1:n], trend)
    colnames(datmat)[1:4] <- c("y", "intercept", "y.l1", "trend")
    if (lag > 0) {
        for (i in 1:lag) {
            datmat[, i + 3] <- c(rep(NA, i + 1), diff(y))[1:n]
        }
        colnames(datmat) <- c("y", "y.l1", "trend", paste("y.dl", 1:lag, sep = ""))
    }
    if (model == "intercept") {
        roll <- function(z) {
            du <- c(rep(0, z), rep(1, (n - z)))
            rollmat <- cbind(datmat, du)
            roll.reg <- tryCatch(fastLmPure(rollmat[2:dim(rollmat)[1], 2:dim(rollmat)[2]], rollmat[2:dim(rollmat)[1], 1]))
            if (class(roll.reg) != "list") {
                roll.reg <- coef(summary(lm(as.data.frame(rollmat))))
                (roll.reg[2, 1] - 1)/roll.reg[2, 2]
            } else {
                (roll.reg$coefficients[2] - 1)/roll.reg$stderr[2]
            }
        }
        roll.stat <- sapply(idx, roll)
        cval <- c(-5.34, -4.8, -4.58)
        bpoint <- which.min(roll.stat)
    } else if (model == "trend") {
        roll <- function(z) {
            dt <- c(rep(0, z), 1:(n - z))
            rollmat <- cbind(datmat, dt)
            roll.reg <- tryCatch(fastLmPure(rollmat[2:dim(rollmat)[1], 2:dim(rollmat)[2]], rollmat[2:dim(rollmat)[1], 1]))
            if (class(roll.reg) != "list") {
                roll.reg <- coef(summary(lm(as.data.frame(rollmat))))
                (roll.reg[2, 1] - 1)/roll.reg[2, 2]
            } else {
                (roll.reg$coefficients[2] - 1)/roll.reg$stderr[2]
            }
        }
        roll.stat <- sapply(idx, roll)
        cval <- c(-4.93, -4.42, -4.11)
        bpoint <- which.min(roll.stat)
    } else if (model == "both") {
        roll <- function(z) {
            du <- c(rep(0, z), rep(1, (n - z)))
            dt <- c(rep(0, z), 1:(n - z))
            rollmat <- cbind(datmat, du, dt)
            roll.reg <- tryCatch(fastLmPure(rollmat[2:dim(rollmat)[1], 2:dim(rollmat)[2]], rollmat[2:dim(rollmat)[1], 1]))
            if (class(roll.reg) != "list") {
                roll.reg <- coef(summary(lm(as.data.frame(rollmat))))
                (roll.reg[2, 1] - 1)/roll.reg[2, 2]
            } else {
                (roll.reg$coefficients[2] - 1)/roll.reg$stderr[2]
            }
        }
        roll.stat <- sapply(idx, roll)
        cval <- c(-5.57, -5.08, -4.82)
        bpoint <- which.min(roll.stat)
    }
    teststat <- roll.stat[bpoint]
    results = list(teststat = teststat, cval = cval, bpoint = bpoint)
    return(results)
} 
