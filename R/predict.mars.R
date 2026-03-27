#' predict.mars
#'
#' @param object mars object of model
#' @param newdata new data of predictors
#' @param ... additional arguments
#'
#' @returns predicted values
#'
#' @export
#'
#' @examples
#' set.seed(2026)
#' x1 <- 1:100
#' x2 <- rnorm(100)
#' x3 <- rnorm(100)
#' y <- pmax(0, x1 - 0.5)*pmax(0, x2 - 0.5)  + rnorm(100)
#' data <- data.frame(y,x1,x2,x3)
#' newdata <- data[,-1] # drop response y
#' fit <- mars(y ~ ., data = data, control = mars.control(Mmax=4))
#' predict(fit,newdata)
predict.mars <- function(object,newdata,...) {
  if(missing(newdata) || is.null(newdata)) {
    B <- as.matrix(object$B)
  }
  else {
    tt <- terms(object$formula,data=newdata)
    tt <- delete.response(tt)
    mf <- model.frame(tt,newdata)
    mt <- attr(mf, "terms")
    X <- model.matrix(mt, mf)[,-1] # remove intercept
    B <- make_B(X,object$Bfuncs)
  }
  beta <- object$coefficients
  drop(B %*% beta)
}

#' New basis functions
#'
#' @param X new predictors
#' @param Bfuncs basis functions
#'
#' @returns new basis functions
make_B <- function(X, Bfuncs){

  N <- nrow(X)
  Mmax <- length(Bfuncs) -1
  B <- matrix(1, nrow = N, ncol = Mmax + 1) # Recall that B is an N x (Mmax+1) matrix
  for (m in 2:ncol(B)){ # Loop through each non-constant basis functions
    for (i in 1:nrow(Bfuncs[[m]])){ # Loop through each branch (hinge function)
      v <- Bfuncs[[m]][i,"v"]
      s <- Bfuncs[[m]][i,"s"]
      t <- Bfuncs[[m]][i,"t"]
      B[,m] <- B[,m]*h(X[,v], s, t)  # Update B[,m] using hinge function and new data on covariates.
    }
  }
  # To do: replace ??? with correct values or equations
  return(B)
}

#' Hinge Function
#'
#' @param x vector of predictors
#' @param s +/- hinge
#' @param t split location
#'
#' @returns maximum of inputs
h <- function(x,s,t) {
  return(pmax(0,s*(x-t)))
  # if x>t, s=+1, this return max(0,x-t)
  # if x<t, s=-1, this return max(0,t-x)
}
