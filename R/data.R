#' Wide format data
#'
#' @format A data frame with 60 rows and 5 variables:
#' \describe{
#'   \item{subject}{Subject.}
#'   \item{treatment}{Treatment.}
#'   \item{T0}{Score at baseline.}
#'   \item{T3}{Score at 3th months.}
#'   \item{T6}{Score at 6th months.}
#' }
"wdata"


#' Long format data
#'
#' @format A data frame with 180 rows and 4 variables:
#' \describe{
#'   \item{subject}{Subject.}
#'   \item{treatment}{Treatment.}
#'   \item{time}{Time at baseline, 3 months, and 6 months.}
#'   \item{score}{Score.}
#' }
"ldata"
