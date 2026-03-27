#' print.mars objects
#'
#' @param x mars object
#' @param ... additional arguments
#'
#' @returns call and coefficients of mars output
#'
#' @export
#' @examples
#' set.seed(2026)
#' x1 <- 1:100
#' x2 <- rnorm(100)
#' x3 <- rnorm(100)
#' y <- pmax(0, x1 - 0.5)*pmax(0, x2 - 0.5)  + rnorm(100)
#' data <- data.frame(y,x1,x2,x3)
#' fit <- mars(y ~ ., data = data, control = mars.control(Mmax=4))
#' print(fit)
print.mars<- function(x,...) {

  cat("Call:\n")
  print(x$call)

  cat("\nCoefficients:\n")
  print(x$coefficients)

}

