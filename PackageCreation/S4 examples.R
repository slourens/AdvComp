library(lme4)

sampSize <- 100

dfList <- lapply(1:sampSize, function(t) {
  nobs <- round(runif(1, 0.5, 8.5))
  b0 <- rnorm(1, 0, 1.5)
  if (nobs == 1)
  {
    x <- 0
  } else {
    xtail <- rnorm(nobs-1, 1:nobs, 0.1)
    x <- c(0, xtail)
  }
  y <- rnorm(nobs, 3 + 2 * x + 0.25 * x ^ 2, 1)
  id <- rep(t, nobs)
  dfCurr <- data.frame(id, y, x)
  return(dfCurr)
})

dfFinal <- do.call('rbind', dfList)

lmmTest <- lmer(y ~ x + (1 | id), data = dfFinal)

## define new generic for accessing gradient/Hessian at optimum
accGradHess <- function(lmerMod)
{
  optQuants <- lmerMod@optinfo$derivs
  grad <- optQuants[[1]]
  hess <- optQuants[[2]]
  return(list(grad = grad, hess = hess))
}

setGeneric("accGradHess", function(x) {
  standardGeneric("accGradHess")
})

setMethod("accGradHess", "lmerMod", accGradHess)


setClass("newTuples", slots = list(nrow = "numeric"), contains = c("matrix"))

newTuples <- function(data=matrix(ncol = 2), nrow = 1)
{
  if (dim(data)[2] != 2)
  {
    stop("The data needs to have 2 columns to be a tuple!")
  } else if(!inherits(data, "matrix")) {
    stop("Argument data must inherit from class matrix!")
  } else {
    nrow <- dim(data)[1]
    new("newTuples", data, nrow = nrow)
  }
}

