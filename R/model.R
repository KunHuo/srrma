#' Repeated measures ANOVA for long format data
#'
#' @param data a data.frame is in the so-called long format.
#' @param between between-subject variables.
#' @param within within-subjects variables.
#' @param response dependent variable.
#' @param covariates covariates.
#' @param subject a subject is an observational unit that can be considered independent of other subjects.
#' @param interaction a logical variable specifying whether to include interaction terms.
#'
#' @return a object of repeated measures ANOVA.
#' @export
#'
#' @examples
#' repeated_measures_long(data = ldata,
#'                        between = treatment,
#'                        within = time,
#'                        response = score,
#'                        subject = subject)
#'
#' repeated_measures_long(data = ldata,
#'                        between = "treatment",
#'                        within = "time",
#'                        response = "score",
#'                        subject = "subject")
repeated_measures_long <- function(data,
                          between = NULL,
                          within = NULL,
                          response = NULL,
                          covariates = NULL,
                          subject = NULL,
                          interaction = TRUE){

  subject    <- tidyselect::vars_select(names(data), !!dplyr::enquo(subject))
  between    <- tidyselect::vars_select(names(data), !!dplyr::enquo(between))
  within     <- tidyselect::vars_select(names(data), !!dplyr::enquo(within))
  response   <- tidyselect::vars_select(names(data), !!dplyr::enquo(response))
  covariates <- tidyselect::vars_select(names(data), !!dplyr::enquo(covariates))

  data[[between]] <- factor(data[[between]])
  data[[within]]  <- factor(data[[within]])

  sep <- ifelse(interaction, "*", "+")
  frm <- sprintf("%s ~ %s %s %s + Error(%s/%s)", response, between, sep, within, subject, within)
  frm <- stats::as.formula(frm)
  afex::aov_car(formula = frm, data = data)
}



#' Repeated measures ANOVA for wide format data
#'
#' @param data a data.frame is in the so-called wide format.
#' @param between between-subject variables.
#' @param responses dependent variable.
#' @param covariates covariates.
#' @param subject a subject is an observational unit that can be considered independent of other subjects.
#' @param interaction a logical variable specifying whether to include interaction terms.
#'
#' @return a object of repeated measures ANOVA.
#' @export
#'
#' @examples
#' repeated_measures_wide(data = wdata,
#'                        subject = subject,
#'                        between = treatment,
#'                        responses = T0:T6)
repeated_measures_wide <- function(data,
                          between = NULL,
                          responses = NULL,
                          covariates = NULL,
                          subject = NULL,
                          interaction = TRUE) {

  subject    <- tidyselect::vars_select(names(data), !!dplyr::enquo(subject))
  between    <- tidyselect::vars_select(names(data), !!dplyr::enquo(between))
  responses  <- tidyselect::vars_select(names(data), !!dplyr::enquo(responses))
  covariates <- tidyselect::vars_select(names(data), !!dplyr::enquo(covariates))

  if(length(subject) == 0L){
    data <- data %>%
      dplyr::mutate(subject = 1:nrow(data)) %>%
      dplyr::relocate(subject)
    subject <- "subject"
  }

  ldata <- tidyr::pivot_longer(data,
                               cols = responses,
                               names_to = "within",
                               values_to = "response")

  repeated_measures_long(data = ldata,
                subject = subject,
                between = between,
                within = "within",
                response = "response",
                covariates = covariates,
                interaction = interaction )
}


