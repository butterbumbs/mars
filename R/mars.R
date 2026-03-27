#' Multivariate Adaptive Regression Splines (MARS)
#'
#' `mars` fits a model on data to capture a non-linear relationship. Using step-functions between knots, it splits basis functions and removes those that don't improve the model. Returning the optimal number of basis functions.
#'
#' @param formula object of formula to regress a response such as `y` and a predictor such as `x`. Use `.` for all in data
#' @param data input data frame
#' @param control mars.control object - choice of Mmax
#'
#' @details
#' A fitted model using `mars`, primarily go through three stages. First, when a call is made, it goes through a forward pass stepwise selection of basis functions in `fwd_stepwise()`, which creates splits on basis functions and optimizes these to find the best splits. Second, the results of fwd_stepwise, are passed to a backward stepwise selection of basis functions in `bwd_stepwise()`, which prunes basis functions by removing those that do not improve the model by checking Generalized Cross-Validation (GCV) and when GCV can no longer be reduced, we have our output. The final stage returns the results of the best basis functions, with their variable/predictor, sign and knot of their split. The output returned is augmented by additional parameters from `mars.control().`
#'
#' @returns an object of class `mars`, inheriting from class `lm`. Returns a list of class `lm` plus a list of class `mars.
#'
#' Some `lm` values:<br>
#' `coefficients`.. holds coefficients on model<br>
#' `rank`.......... rank of basis function<br>
#' `residuals`..... error terms<br>
#' `etc`........... additional `lm` values
#'
#' additional to this list in `mars` is:<br>
#' `formula`....... object of formula<br>
#' `call`.......... call of formula matched<br>
#' `y`............. data frame of response<br>
#' `B`............. basis functions<br>
#' `Bfuncs`........ optimize basis functions<br>
#' `x_names`....... names of predictors<br>
#'
#' @author
#' Zvikomborero Kennedy Jokonya | email: zkj@sfu.ca | github: @butterbumbs |
#'
#' @references
#' Jerome H. Friedman "Multivariate Adaptive Regression Splines," The Annals of Statistics, Ann. Statist. 19(1), 1-67, (March, 1991)
#'
#' @seealso
#' \code{\link[=print.mars]{print}},
#' \code{\link[=summary.mars]{summary}},
#' \code{\link[=predict.mars]{predict}},
#' \code{\link[=plot.mars]{plot}}
#'
#'
#' @import stats
#'
#' @export
#'
#' @examples
#' # Example 1:
#'
#' set.seed(2026)
#' x1 <- 1:100
#' x2 <- rnorm(100)
#' x3 <- rnorm(100)
#' y <- pmax(0, x1 - 0.5)*pmax(0, x2 - 0.5)  + rnorm(100)
#' data <- data.frame(y,x1,x2,x3)
#' fit <- mars(y ~ ., data = data, control = mars.control(Mmax=4))
#' print(fit)
#' summary(fit)
#'
#' newdata <- data[,-1] # drop response y
#' predict(fit,newdata)
#'
#' anova(fit)
#'
#' plot(fit)
#'
#' # Example 2: Economic data
#' # Using ISLR data to find the best predictor of wage
#'
#' Wage <- ISLR::Wage[,-10] # drop logwage or replace wage with logwage as the response
#'
#' fit <- mars(wage ~. , Wage, control = mars.control(Mmax=6))
#' summary(fit)
#'
#' anova(fit)
#'
#' # Example 3: Housing data
#' # Using MASS data of housing prices
#'
#' boston <- MASS::Boston
#' fit <- mars(medv ~ ., boston, control = mars.control(Mmax=6))
#' summary(fit)
#'
#' anova(fit)
#'
#' plot(fit)
mars <- function(formula,data,control=mars.control()) {
  cc <- match.call() # save the call
  mf <- model.frame(formula,data)
  y <- model.response(mf)
  mt <- attr(mf, "terms")
  x <- model.matrix(mt, mf)[,-1,drop=FALSE]
  x_names <- colnames(x)
  validate_mars.control(control)
  fwd <- fwd_stepwise(y,x,control)
  bwd <- bwd_stepwise(fwd,control)  # If you opt to not get the bonus point, you can simply ignore this line.

  fit <- lm(y ~ .-1, data=data.frame(y = y, bwd$B))
  out <- c(
    list(call=cc,
         formula=formula,
         y=y,
         B=bwd$B,
         Bfuncs=bwd$Bfuncs,
         x_names=x_names),
    fit
  )

  class(out) <- c("mars",class(fit))
  out
}

#' Forward stepwise
#'
#' @param y vector of dependent/response variables
#' @param x matrix of independent/predictor variables
#' @param control mars.control object
#'
#' @returns list with y, B and Bfuncs
fwd_stepwise <- function(y,x,control=mars.control()) {

  mc <- control

  # Error checking for Mmax:
  if (mc$Mmax < 2) {
    Mmax <- 2
    warning("Mmax NOT Greater nor Equal to 2. Setting Mmax to 2.")
  }

  # Initialize:
  N <- length(y) # sample size
  n <- ncol(x) # number of predictors

  # To do: Initialize B with your init_B() function and Bfuncs to be an empty list of length mc$Mmax+1

  B <- init_B(N,mc$Mmax)

  Bfuncs <- vector(mode = "list", length = mc$Mmax+1)


  #---------------------------------------------------
  for(i in seq(1, mc$Mmax, by=2)) {
    # To do: Replace the M loop with a loop over pairs i

    M <- i           # To do: Set the value of M from the value of i.

    if(control$trace) cat("M",M,"\n")

    lof_best <- Inf

    for(m in 1:M) { # choose a basis function to split

      # To do: Create an object "svars" to store the indices of variables that are not already in basis function m.

      vars <- Bfuncs[[m]][,"v"]
      svars <- setdiff(1:n, vars)                                      ####

      if(control$trace) cat("M, m, svars", M, m, svars,"\n")

      for(v in svars){ # select a variable to split on

        tt <- split_points(x[,v],B[,m])

        for(t in tt) {

          # To do: Change the way of creating "Bnew" so that we do not remove the parent basis function, and we add pairs of child basis functions using the hinge function defined previously.

          Bnew <- data.frame(
            B[,(1:M)],
            Btem1=B[,m]*h(x[,v], -1, t),
            Btem2=B[,m]*h(x[,v], 1, t)
          )

          gdat <- data.frame(y=y,Bnew)
          lof <- LOF(y~., gdat, control)

          if(lof < lof_best) {
            lof_best <- lof
            split_best <- c(m=m,v=v,t=t)
          }
        }
      }
    }
    mstar <- split_best["m"]
    vstar <- split_best["v"]
    tstar <- split_best["t"]

    cat("[Info] best (m,v,t,lof): (",mstar,vstar,tstar,lof_best,")\n")

    # To do: Update Bfuncs and B according to Algorithm 2 - forward stepwise in Friedman's paper.

    # update B
    B[ ,M+1] <- B[,mstar]*h(x[,vstar], -1, tstar)
    B[ ,M+2] <- B[,mstar]*h(x[,vstar], 1, tstar)

    Bfuncs[[M+1]] <- rbind(Bfuncs[[mstar]], c(s=-1, vstar, tstar))

    Bfuncs[[M+2]] <- rbind(Bfuncs[[mstar]],c(s=+1, vstar, tstar))



  } # end for loop over i

  # To do: Set the column names of B.
  colnames(B) <- paste0("B", 0:(ncol(B) - 1))

  # To do: Return the list 'list(y=y, B=B, Bfuncs=Bfuncs)'.
  return(list(y=y,B=B,Bfuncs=Bfuncs))
}


#' Initialize basis functions
#'
#' @param N sample size
#' @param Mmax maximum of basis functions
#'
#' @returns data frame of N x (Mmax+1)
init_B <- function(N,Mmax) {
  B <- data.frame(matrix(NA,nrow=N,ncol=Mmax+1))
  B[,1] <- 1
  return(B)
}


#' Backward stepwise
#'
#' @param fwd from fwd_stepwise
#' @param control mars.control object
#'
#' @returns list with y, B and Bfuncs
bwd_stepwise <- function(fwd,control) {

  y <- fwd$y
  B <- fwd$B
  Mmax <- ncol(fwd$B)
  Jstar <- 2:Mmax
  Kstar <- Jstar
  dat <- data.frame(y, B)
  lofstar <- LOF(y~.-1,dat,control)
  for(M in (Mmax-1):1) {
    b <- Inf
    L <- Kstar
    if(control$trace) cat("L:",L,"\n")
    for(m in L){
      K <- setdiff(L,m)
      dat <- data.frame(y, B[,K])
      lof <- LOF(y~.,dat,control)
      if(control$trace) cat("M:K:lof",M,":",K,":",lof,"\n")
      if(lof < b) {
        b <- lof
        Kstar <- K
      }
      if(lof < lofstar) {
        lofstar <- lof
        Jstar <- K
      }
    }
    if(control$trace) cat("M:Jstar:lofstar",M,":",Jstar,":",lofstar,"\n")
  }
  Jstar <- c(1,Jstar)

  return(list(y=fwd$y,B=fwd$B[,Jstar],Bfuncs=fwd$Bfuncs[Jstar]))
}


#' Lack of Fit (LOF)
#'
#' @param form object of formula
#' @param data input data frame
#' @param control mars.control object
#'
#' @returns value of GCV
LOF <- function(form,data,control) {
  ff <- lm(form,data)
  RSS <- sum(residuals(ff)^2)
  N <- nrow(data)
  M <- length(coef(ff))-1 # GCV formula includes the number of non-constant basis functions
  Ctilde <- sum(diag(hatvalues(ff))) + control$d*M # control$d*M is the penalty term
  return(RSS * N/(N-Ctilde)^2)
}


#' Hinge Function
#'
#' @param x vector of predictors
#' @param s +/- hinge
#' @param t split location
#'
#' @returns maximum of inputs
h <- function(x,s,t) {
  return(pmax(0, s*(x-t)))
}


#' Split Points
#'
#' @param x vector of predictors
#' @param B basis function
#'
#' @returns sorted split points on x
split_points <- function(x, B) {
  x <- x[B > 0]
  x <- sort(unique(x))
  x <- x[-length(x)]
  return(x)
}

#------------------------------------------------------------------------
# constructor, validator and helper for class mars.control
#

#' Helper
#'
#' @param control mars.control object
new_mars.control <- function(control) {
  structure(control, class="mars.control")
}


#' Validator
#'
#' @param control mars.control object
#'
#' @returns control
validate_mars.control <- function(control) {
  stopifnot(
    is.integer(control$Mmax),
    is.numeric(control$d),
    is.logical(control$trace)
  )
  if(control$Mmax < 2) {
    warning("Mmax must be >= 2; Reset it to 2")
    control$Mmax <- 2}
  if(control$Mmax %% 2 > 0) {
    control$Mmax <- 2*ceiling(control$Mmax/2)
    warning("Mmax should be an even integer. Reset it to ",control$Mmax)
  }
  control
}

#' Constructor for `mars.control` objects
#'
#' This function constructs a `mars.control` object that specifies
#' parameters used in the model fitting procedure.
#'
#' @param Mmax Maximum number of basis functions. Should be an even integer. Default value is 2.
#' @param d The coefficient in the penalty term of the generalized cross validation measure. Default is 3.
#' @param trace Should we print status information about the fitting? Default is `FALSE`
#'
#' @return a `mars.control` object
#' @export
mars.control <- function(Mmax=2,d=3,trace=FALSE) {
  Mmax <- as.integer(Mmax)
  control <- list(Mmax=Mmax,d=d,trace=trace)
  control <- validate_mars.control(control)
  new_mars.control(control)
}
