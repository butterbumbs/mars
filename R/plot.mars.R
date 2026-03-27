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

  object <- x

  call <- object$call
  formula <- eval(call$formula, parent.frame())
  data <- eval(call$data, parent.frame())

  x <- model.matrix(
    attr(model.frame(formula,data), "terms"),
    model.frame(formula,data))[,-1,drop=FALSE]

  for(m in 2:length(object$Bfuncs)) {

    BF = object$Bfuncs[[m]]
    xv = sort(x[,BF[1, "v"]])
    yv = h(xv,BF[1, "s"],BF[1, "t"])

    switch(
      length(unique(BF[, "v"])), {
        plot(xv, yv, type="l",
             main = colnames(object$B)[m],
             xlab = object$x_names[BF[1,"v"]],
             ylab = object$formula[[2]], ...)
        },
        {
          xvx = seq(min(x[, BF[1, "v"]]),max(x[, BF[1, "v"]]),length.out = n)
          xvy = seq(min(x[, BF[2, "v"]]),max(x[, BF[2, "v"]]),length.out = n)
          xvz = outer(xvx,xvy,
                      function(xvx,xvy)
                        h(xvx, BF[1, "s"], BF[1, "t"]) * h(xvy, BF[2, "s"], BF[2, "t"]))

          graphics::persp(xvx,xvy,xvz,col=col, border=border,
                xlab=object$x_names[BF[1, "v"]], ylab=object$x_names[BF[2, "v"]], zlab= object$formula[[2]],
                main=colnames(object$B)[m], theta=theta, phi=phi)
        }
      )
    }
  invisible(x)
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




