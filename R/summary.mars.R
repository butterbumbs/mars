#' summary.mars
#'
#' @param object mars object
#' @param ... additional arguments
#'
#' @returns summary of mars output
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
#' fit <- mars(y ~ ., data = data, control = mars.control(Mmax=4))
#' summary(fit)
summary.mars <- function(object,...) {

  cat("Basis Functions:\n")

  for(m in 1:length(object$Bfuncs)) {
    cat(" ", colnames(object$B)[m], ":",
        switch(is.null(object$Bfuncs[[m]]) + 1,
               paste0(
                 " (", "Variable = ", object$x_names[object$Bfuncs[[m]][,"v"]], "; ",
                 "Sign = ", object$Bfuncs[[m]][,"s"], "; ",
                 "Knot = ", round(object$Bfuncs[[m]][,"t"], 4), ")",
                 collapse = " *"
                 ), " Intercept"
               ), "\n"
    )
    }

  summary.lm(object)

}


