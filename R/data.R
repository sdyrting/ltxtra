#' Life Tables of Australian States
#'
#' Life tables of Australian states by sex.
#'
#' @format ## `aus_2021_2023`
#' A data frame with 1818 rows and 12 columns
#' \describe{
#'    \item{Stat}{Indigenous status. `Aboriginal` or `Non-Aboriginal`}
#'    \item{Sex}{Sex. `Female` or `Male`}
#'    \item{Age}{Age in single years form 0 to 100. The last age group is open.}
#'    \item{mx}{Death rate}
#'    \item{qx}{Death probability}
#'    \item{ax}{Separation factor. The average number of additional years lived by those who died at this age}
#'    \item{lx}{Survival fraction}
#'    \item{dx}{Deaths}
#'    \item{Lx}{Person years lived}
#'    \item{Tx}{Person years lived after}
#'    \item{ex}{Life expectancy}
#'    \item{OpenInterval}{Logical. Flogs whether an interval is open (`TRUE`) or closed (`FALSE`)}
#' }
"aus_2021_2023"
