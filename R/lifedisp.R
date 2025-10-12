# Add functions for calculating life disparity and related measures

#' Adds a life disparity column to a life table
#'
#' @param lt A life table
#' @param method  The approximatiom methed. Can be `'sqa'`, `'enu'`, `'els'` or `'els'`. Defaults to `'sqa'`.
#' @returns A life table with the extra column `vx` giving the life disparity at age `x`
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr lead
#' @importFrom dplyr lag
#' @importFrom dplyr select
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' library(dplyr)
#' daus_lt <- aus_2021_2023 %>% group_by(State,Sex) %>% lifedisp()
lifedisp <- function(lt,method='sqa') {
  
  BIGAGE <- 200
  
  ans <- NA
  
  if (method == 'enu') {
    ans <- lt %>%
      dplyr::mutate(
        .lx = dplyr::lag(cumprod(1-.data$qx),default=1),
        .dx = .data$.lx-dplyr::lead(.data$.lx,default=0),
        .nx = dplyr::lead(.data$Age,default=BIGAGE)-.data$Age,
        vx = ifelse(.data$OpenInterval,
                               0.5*.data$.dx*.data$ex,
                               .data$.dx*(.data$.nx-.data$ax+dplyr::lead(.data$ex,default=0))),
        vx = rev(cumsum(rev(.data$vx)))/.data$.lx
      ) %>%
      dplyr::select(!c(.data$.lx,.data$.dx,.data$.nx))
  } else if (method == 'els') {
    ans <- lt %>%
      mutate(.lx = lag(cumprod(1-.data$qx),default=1),
             .dx = .data$.lx-lead(.data$.lx,default=0),
             vx = ifelse(.data$OpenInterval,
                               0.5*.data$.dx*.data$ex,
                               0.5*.data$.dx*(.data$ex+lead(.data$ex,default=0))),
             vx = rev(cumsum(rev(.data$vx)))/.data$.lx
      ) %>%
      dplyr::select(!c(.data$.lx,.data$.dx))
  } else if (method == 'ela') {
    ans <- lt %>%
      mutate(.lx = lag(cumprod(1-.data$qx),default=1),
             .dx = .data$.lx-lead(.data$.lx,default=0),
             .nx = lead(.data$Age,default=BIGAGE)-.data$Age,
             vx = ifelse(.data$OpenInterval,.data$.dx*.data$ex,
                               .data$.dx*(.data$ex+.data$ax/.data$.nx*c(diff(.data$ex),0))),
             vx = rev(cumsum(rev(.data$vx)))/.data$.lx) %>%
      dplyr::select(!c(.data$.lx,.data$.dx,.data$.nx))
  } else if (method == 'sqa') {
    ans <- lt %>% lifedisp_sqa()
  } else {
    stop(paste0('Unknown approximation method: ',method))
  }
  
  return(ans)
}

#' Adds a life disparity column to a life table
#'
#' @param lt A life table
#'
#' @returns A life table with life disparity calculated using the SQA method
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr lead
#' @importFrom dplyr lag
#' @importFrom dplyr select
#' @importFrom rlang .data
#' @noRd
lifedisp_sqa <- function(lt) {
  
  BIGAGE <- 200
  
  ans <- lt %>% 
    dplyr::mutate(
      .nx = dplyr::lead(.data$Age,default=BIGAGE)-.data$Age,
      .lx = dplyr::lag(cumprod(1-.data$qx),default=1),
      .dx = .data$.lx-dplyr::lead(.data$.lx,default=0),
      .Lx = .data$.nx*.data$.lx-(.data$.nx-.data$ax)*.data$.dx,
      .Tx = rev(cumsum(rev(.data$.Lx))),
      .OV = ifelse(.data$OpenInterval,olifedisp(lag(.data$qx),lag(.data$.nx),.data$ex),0),
      .V= ifelse(.data$OpenInterval,.data$.OV*.data$.lx,vfunc(.data$.Lx,.data$.nx,.data$.lx,.data$OpenInterval)),
      .W= .data$.V+dplyr::lead(.data$.Tx,default=0)*log(.data$.lx/dplyr::lead(.data$.lx,default=1)),
      vx=rev(cumsum(rev(.data$.W)))/.data$.lx) %>%
    dplyr::select(!c(.data$.nx,.data$.lx,.data$.dx,.data$.Lx,.data$.Tx,.data$.OV,.data$.V,.data$.W))
  
  return(ans)
}

#' Returns life disparity at the open interval using Gompertz model
#'
#' @param q Death probability for the last closed interval
#' @param n n Age interval for the last closed interval
#' @param olifeexp olifeexp Life expectancy at the start of the open interval
#'
#' @returns life disparity
#'
#' @examples
#' olifedisp(0.3,5,10)
#' @noRd
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

#' Returns V used in backwards recurrence calculation of life disparity
#'
#' @param YearsLived Years lived over the age interval
#' @param AgeInterval Length in years of the age interval
#' @param SurvFrac Survival fraction at the start of the age interval
#'
#' @returns V = int_x1^x2 l(y) log(l1/l(y)) dy
#' @importFrom dplyr lead
#' 
#' @noRd
vfunc <- function(YearsLived,AgeInterval,SurvFrac,OpenInterval) {
  
  ClosedInterval <- !OpenInterval
  V <- YearsLived
  C <- (YearsLived-0.5*AgeInterval*(SurvFrac+dplyr::lead(SurvFrac,default=0)))/(SurvFrac-dplyr::lead(SurvFrac,default=0))/AgeInterval
  q <- 1- dplyr::lead(SurvFrac,default=0)/SurvFrac
  A <- q*(1-6*C)
  B <- 6*q*C
  V[ClosedInterval] <- vfunc_sqa(A[ClosedInterval],B[ClosedInterval])
  V[OpenInterval] <- NA
  V <- AgeInterval*SurvFrac*V
  
  return(V)
}

#' Returns normalised V from the A and B parameters of the SQA approximation
#'
#' @param A Linear coefficient of the survival fraction
#' @param B Quadratic coefficient of the survival fraction
#'
#' @returns Normalised V. V = Normalised V x AgeInterval x SurvFrac
#' 
#' @noRd
vfunc_sqa <- function(A,B) {
  
  TINY <- 1.0e-9
  
  if (any(A+B >= 1)) warning('A+B < 1 violated')
#  if (any(A <= 0)) warning('A > 0 violated')
#  if (any(A+2*B <= 0)) warning('A+2B > 0 violated')
#  v_not_defined <- (A+B >= 1 | A <= 0 | A + 2*B <= 0)
 
  # C0: V not defined
  
suppressWarnings(
  # C1: B > 0
  R1 <- ((sqrt(4*B+A^2)*((4*B+A^2)*log(abs(sqrt(4*B+A^2)+A))+(-(4*B)-A^2)*log(abs(sqrt(4*B+A^2)-A))))/B^2)/12-
    ((sqrt(4*B+A^2)*((12*B+3*A^2)*log(abs(sqrt(4*B+A^2)+2*B+A))+(-(12*B)-3*A^2)*log(abs(sqrt(4*B+A^2)-2*B-A)))+
        (-(12*B^3)+(36-18*A)*B^2+18*A*B+3*A^3)*log(abs(B+A-1))+8*B^3+(12*A-48)*B^2-6*A^2*B)/B^2)/36  
)
  
  # C2: B = 0
suppressWarnings(
  R2 <- ((2*log(abs(A-1))-1)*A^2+(2-4*log(abs(A-1)))*A+2*log(abs(A-1)))/(4*A)
)
  # C3.1: B < 0, A^2+4*B > 0
suppressWarnings(
  R3.1 <- ((sqrt(4*B+A^2)*((4*B+A^2)*log(abs(sqrt(4*B+A^2)+A))+(-(4*B)-A^2)*log(abs(sqrt(4*B+A^2)-A))))/B^2)/12-
    ((sqrt(4*B+A^2)*((12*B+3*A^2)*log(abs(sqrt(4*B+A^2)+2*B+A))+(-(12*B)-3*A^2)*log(abs(sqrt(4*B+A^2)-2*B-A)))+
        (-(12*B^3)+(36-18*A)*B^2+18*A*B+3*A^3)*log(abs(B+A-1))+8*B^3+(12*A-48)*B^2-6*A^2*B)/B**2)/36
)
  # C3.2: B < 0, A^2+4*B < 0
suppressWarnings(
  R3.2 <- (((12*B^3+(18*A-36)*B^2-18*A*B-3*A^3)*log(abs(B+A-1))+
              sqrt(-(4*B)-A^2)*(2*(12*B+3*A^2)*atan((sqrt(-(4*B)-A^2)*(2*B+A))/(4*B+A^2)))-
              8*B^3+(48-12*A)*B^2+6*A^2*B)/B^2)/36-
    ((sqrt(-(4*B)-A^2)*(4*B+A^2)*atan((A*sqrt(-(4*B)-A^2))/(4*B+A^2)))/B^2)/6
)
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
#  ans[v_not_defined] <- NA
  
  return(ans)  
}
