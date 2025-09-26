# Add functions that other functions might use
ogompertz <- function(q,n,olifeexp) {
  
  MAXITS <- 20
  RELERR <- 1.0e-6
  
  dx= -n
  
  alpha <- -log(1-q);
  beta <- olifeexp
  
  mu <- alpha/abs(dx)
  dr <- 1.0/beta
  ratio <- -dx/alpha/beta;
  
  if(is.na(ratio)) return(NA)
  
  if (ratio <= 1.0) {#A solution with a > 0 does not exist. Use constant force model
    ans$Ro <- 1.0/beta
    ans$ao <- 0.0
    ans$co <- 1.0
    ans$bo <- INF
    ans$errflag <- FALSE
    and$errmsg <- ''
    return(ans)
  }
  
  astart <- (dr-mu)/mu/abs(dx)
  
  a <- astart
  for (j in 1:5) {#Iterate initial guess
    xa <- exp(a*dx)
    theta <- alpha/(1.0-xa)
    e1 <- expint(theta,scale=TRUE)
    a <- e1/beta
  }
  
  
  for (i in 1:MAXITS)  {#Newton
    xa <- exp(a*dx)
    theta <- alpha/(1.0-xa)
    dtheta <- theta*(xa/(1.0-xa))*dx
    e1 <- expint(theta,scale=TRUE)
    de1 <- dtheta*(e1-1.0/theta)
    f <- e1/a-beta
    df <- (de1*a-e1)/(a*a)
    da <- -f/df
    a <- a+da
    a <- abs(a)
    if (abs(da) < RELERR*abs(a)) {
      xa <- exp(a*dx)
      theta <- alpha/(1.0-xa)
      ans$theta <- theta
      ans$bo <- exp(theta)
      ans$co <- exp(a)
      ans$ao <- a
      ans$Ro <- theta*a
      ans$errflag <- FALSE
      and$errmsg <- ''
      return(ans)
    }
  }
  
  ans$errflag <- TRUE
  ans$errmsg <- "MAXITS exceeded"
  
  return(ans)
}

