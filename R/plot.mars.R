#' plot.mars
#'
#' @param x mars object
#' @param n length.out for 3D plot
#' @param theta controls horizontal view axis for 3D plot
#' @param phi controls vertical view axis for 3D plot
#' @param col color of 3D plot
#' @param border border color for 3D plot
#' @param ... additional arguments
#'
#' @returns plots of basis functions
#'
#' @importFrom graphics persp
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
#' plot(fit)
plot.mars <- function(x, n = 20, theta = 300, phi = 25, col = "lightblue", border = "black",...) {

  X <- model.matrix(
    attr(model.frame(x$formula,eval(x$call$data)), "terms"),
    model.frame(x$formula,eval(x$call$data)))[,-1,drop=FALSE]

  for(m in 2:length(x$Bfuncs)) {

    BF <- x$Bfuncs[[m]]

    switch(
      length(BF[, "v"]), {
        plot(
          sort(X[,BF[1, "v"]]), h(sort(X[,BF[1, "v"]]),BF[1, "s"],BF[1, "t"]), type="l",
             main = colnames(x$B)[m],
             xlab = x$x_names[BF[1,"v"]],
             ylab = x$formula[[2]], ...)
        },
        {
          xvx <- seq(min(X[, BF[1, "v"]]),max(X[, BF[1, "v"]]),length.out = n)
          xvy <- seq(min(X[, BF[2, "v"]]),max(X[, BF[2, "v"]]),length.out = n)
          xvz <- outer(xvx,xvy,
                      function(xvx,xvy)
                        h(xvx, BF[1, "s"], BF[1, "t"]) * h(xvy, BF[2, "s"], BF[2, "t"]))

          graphics::persp(
            xvx,xvy,xvz,col=col, border=border,
            xlab=x$x_names[BF[1, "v"]], ylab=x$x_names[BF[2, "v"]], zlab=x$formula[[2]],main=colnames(x$B)[m], theta=theta, phi=phi)
        }
      )
  }
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




