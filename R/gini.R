#' Adds a gini index  column to a life table
#'
#' @param lt A life table
#'
#' @returns A life table with gini index calculated using the SQA method
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr lead
#' @importFrom dplyr lag
#' @importFrom dplyr select
#' @importFrom rlang .data
#' @noRd
gini <- function(lt) {
  
  BIGAGE <- 200
  
  ans <- lt %>% 
    dplyr::mutate(
      .nx = dplyr::lead(.data$Age,default=BIGAGE)-.data$Age,
      .lx = dplyr::lag(cumprod(1-.data$qx),default=1),
      .dx = .data$.lx-dplyr::lead(.data$.lx,default=0),
      .Lx = .data$.nx*.data$.lx-(.data$.nx-.data$ax)*.data$.dx,
      .Tx = rev(cumsum(rev(.data$.Lx))),
      .OG = ifelse(.data$OpenInterval,ogini(lag(.data$qx),lag(.data$.nx),.data$ex),0),
      .G= ifelse(.data$OpenInterval,.data$.OG*.data$ex*.data$.lx^2,.data$.lx^2*gfunc(.data$.Lx,.data$.nx,.data$.lx,.data$OpenInterval)),
      gx=rev(cumsum(rev(.data$.G)))/(.data$ex*.data$.lx^2),
      gx=1-.data$gx) %>%
    dplyr::select(!c(.data$.nx,.data$.lx,.data$.dx,.data$.Lx,.data$.Tx,.data$.OG,.data$.G))
  
  return(ans)
}


#' Returns gini index at the open interval using Gompertz model
#'
#' @param q Death probability for the last closed interval
#' @param n n Age interval for the last closed interval
#' @param olifeexp olifeexp Life expectancy at the start of the open interval
#'
#' @returns gini index
#'
#' @examples
#' ogini(0.3,5,10)
#' @noRd
ogini <- function(q,n,olifeexp) {
  
  
  gom <- ogompertz(q,n,olifeexp) # Fit Gompertz model
  
  
  if (is.na(gom$Ro)) return(NA) # Gompertz model not defined
  
  if (gom$a == 0) {# Constant force: gini = 1/2
    ans <- 0.5
    return(ans)
  }
  
  ans <- expint(2.0*gom$z,order=1,scale=TRUE)/gom$a/olifeexp
  
  return(ans)
}

ogini <- Vectorize(ogini)


#' Returns G used in backwards recurrence calculation of gini index
#'
#' @param YearsLived Years lived over the age interval
#' @param AgeInterval Length in years of the age interval
#' @param SurvFrac Survival fraction at the start of the age interval
#'
#' @returns G = 1/l(x1)^2 * int_x1^x2 l(y)^2 dy
#' @importFrom dplyr lead
#' 
#' @noRd
gfunc <- function(YearsLived,AgeInterval,SurvFrac,OpenInterval) {
  
  ClosedInterval <- !OpenInterval
  G <- YearsLived
  C <- (YearsLived-0.5*AgeInterval*(SurvFrac+dplyr::lead(SurvFrac,default=0)))/(SurvFrac-dplyr::lead(SurvFrac,default=0))/AgeInterval
  q <- 1- dplyr::lead(SurvFrac,default=0)/SurvFrac
  A <- q*(1-6*C)
  B <- 6*q*C
  A <- A[ClosedInterval]
  B <- B[ClosedInterval]
  G[ClosedInterval] <- 1-A-2.0*B/3.0+A/2.0+A*B/2.0+(A^2)/3.0+(B^2)/5.0
  G[OpenInterval] <- NA
  G <- AgeInterval*G
  
  return(G)
}