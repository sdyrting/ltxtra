# Add functions for calculating life disparity and related measures

#' Returns life disparity at the open interval using Gompertz model
#'
#' @param q Death probability for the last closed interval
#' @param n n Age interval for the last closed interval
#' @param olifeexp olifeexp Life expectancy at the start of the open interval
#'
#' @returns life disparity
#' @export
#'
#' @examples
#' olifedisp(0.3,5,10)
olifedisp <- function(q,n,olifeexp) {
  
  
  gom <- ogompertz(q,n,olifeexp) # Fit Gompertz model
  
  
  if (is.na(gom$Ro)) return(NA) # Gompertz model not defined

  if (gom$a == 0) {# Constant force: lifedisp == lifeexp
    ans <- olifeexp
    return(ans)
  }
  
  ans <- expint(gom$z,order=2,scale=TRUE)/gom$a
  
  return(ans)
}

olifedisp <- Vectorize(olifedisp)


#' Returns normalised V from the A and B parameters of the SQA approximation
#'
#' @param A Linear coefficient of the survival fraction
#' @param B Quadratic coefficient of the survival fraction
#'
#' @returns Normalised V. V = Normalised V x AgeInterval * SurvFrac
vfunc_sqa <- function(A,B) {
  
  TINY <- 1.0e-9
  
  if (any(A+B >= 1)) warning('A+B < 1 violated')
  if (any(A <= 0)) warning('A > 0 violated')
  if (any(A+2*B <= 0)) warning('A+2B > 0 violated')
  
  # C1: B > 0
  R1 <- ((sqrt(4*B+A^2)*((4*B+A^2)*log(abs(sqrt(4*B+A^2)+A))+(-(4*B)-A^2)*log(abs(sqrt(4*B+A^2)-A))))/B^2)/12-
    ((sqrt(4*B+A^2)*((12*B+3*A^2)*log(abs(sqrt(4*B+A^2)+2*B+A))+(-(12*B)-3*A^2)*log(abs(sqrt(4*B+A^2)-2*B-A)))+
        (-(12*B^3)+(36-18*A)*B^2+18*A*B+3*A^3)*log(abs(B+A-1))+8*B^3+(12*A-48)*B^2-6*A^2*B)/B^2)/36  
  
  # C2: B = 0
  R2 <- ((2*log(abs(A-1))-1)*A^2+(2-4*log(abs(A-1)))*A+2*log(abs(A-1)))/(4*A)
  
  # C3.1: B < 0, A^2+4*B > 0
  R3.1 <- ((sqrt(4*B+A^2)*((4*B+A^2)*log(abs(sqrt(4*B+A^2)+A))+(-(4*B)-A^2)*log(abs(sqrt(4*B+A^2)-A))))/B^2)/12-
    ((sqrt(4*B+A^2)*((12*B+3*A^2)*log(abs(sqrt(4*B+A^2)+2*B+A))+(-(12*B)-3*A^2)*log(abs(sqrt(4*B+A^2)-2*B-A)))+
        (-(12*B^3)+(36-18*A)*B^2+18*A*B+3*A^3)*log(abs(B+A-1))+8*B^3+(12*A-48)*B^2-6*A^2*B)/B**2)/36
  
  # C3.2: B < 0, A^2+4*B < 0
  R3.2 <- (((12*B^3+(18*A-36)*B^2-18*A*B-3*A^3)*log(abs(B+A-1))+
              sqrt(-(4*B)-A^2)*(2*(12*B+3*A^2)*atan((sqrt(-(4*B)-A^2)*(2*B+A))/(4*B+A^2)))-
              8*B^3+(48-12*A)*B^2+6*A^2*B)/B^2)/36-
    ((sqrt(-(4*B)-A^2)*(4*B+A^2)*atan((A*sqrt(-(4*B)-A^2))/(4*B+A^2)))/B^2)/6
  
  # C4: |B| < TINY
  R4 <- R2
  
  k1 <- B > 0
  k2 <- B == 0
  k3.1 <- B < 0 & A^2+4*B > 0
  k3.2 <- B < 0 & A^2+4*B < 0
  k4 <- abs(B) < TINY
  
  ans <- B
  ans[k1] <- R1[k1]
  ans[k2] <- R2[k2]
  ans[k3.1] <- R3.1[k3.1]
  ans[k3.2] <- R3.2[k3.2]
  ans[k4] <- R4[k4]  
  
  return(ans)  
}
