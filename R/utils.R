# Add functions that other functions might use
#' Calibrate Gompertz model to last two age intervals of a life table
#'
#' @param q Death probability for the last closed interval
#' @param n Age interval for the last closed interval
#' @param olifeexp Life expectancy at the start of the open interval
#'
#' @returns A list of Gompertz parameters
#' @importFrom expint expint
#' @export
#'
#' @examples
#' ogompertz(0.3,5,10)
ogompertz <- function(q,n,olifeexp) {
  
  MAXITS <- 20
  RELERR <- 1.0e-6
  
  dx= -n
  
  ans <- list(Ro=NA, # Force at the start of the open interval
              a=NA, # Gompertz slope
              z=NA,   # z = Ro/a 
              expa=NA, # expa = exp(a)
              expz=NA, # expz = exp(z)
              errflag=TRUE,
              errmsg='ogompertz: Undefined result')
  
  alpha <- -log(1-q);
  beta <- olifeexp
  
  mu <- alpha/abs(dx)
  dr <- 1.0/beta
  ratio <- -dx/alpha/beta;
  
  if(is.na(ratio)) return(ans) # Ratio undefined
  
  if (ratio <= 1.0) {#A solution with a > 0 does not exist. Use constant force model
    ans$Ro <- 1.0/beta
    ans$a <- 0.0
    ans$expa <- 1.0
    ans$z <- Inf
    ans$expz <- Inf
    ans$errflag <- FALSE
    ans$errmsg <- ''
    return(ans)
  }
  
  astart <- (dr-mu)/mu/abs(dx)
  
  a <- astart
  for (j in 1:5) {#Iterate initial guess
    xa <- exp(a*dx)
    z <- alpha/(1.0-xa)
    e1 <- expint::expint(z,scale=TRUE)
    a <- e1/beta
  }
  
  
  for (i in 1:MAXITS)  {#Newton
    xa <- exp(a*dx)
    z <- alpha/(1.0-xa)
    dz <- z*(xa/(1.0-xa))*dx
    e1 <- expint::expint(z,scale=TRUE)
    de1 <- dz*(e1-1.0/z)
    f <- e1/a-beta
    df <- (de1*a-e1)/(a*a)
    da <- -f/df
    a <- a+da
    a <- abs(a)
    if (abs(da) < RELERR*abs(a)) {
      xa <- exp(a*dx)
      z <- alpha/(1.0-xa)
      ans$z <- z
      ans$expz <- exp(z)
      ans$a <- a
      ans$expa <- exp(a)
      ans$Ro <- z*a
      ans$errflag <- FALSE
      ans$errmsg <- ''
      return(ans)
    }
  }
  
  ans$errflag <- TRUE
  ans$errmsg <- "ogompertz: MAXITS exceeded"
  
  return(ans)
}

