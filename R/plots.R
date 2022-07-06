#' Plot for repeated measures ANOVA
#'
#' @param model a object from [repeated_measures_long] or [repeated_measures_wide]
#' @param error.type the type of error bar, one of 'se', 'ci', 'sd', and 'none'.
#' @param method a character value giving the root name of a contrast method, see [contrast] or [contrast-methods].
#' @param adjust a character, adjust P-values for multiple comparisons, see [contrast] or [p.adjust].
#' @param ref integer(s) or character(s) specifying which level(s) to use as the reference. Character values must exactly match elements of levs
#' @param signif a logical variable indicating whether to show significance codes.
#' @param error.width width of error bar.
#' @param line.size line size.
#' @param point.size point size.
#' @param vjust.signif vjust for significance codes.
#' @param family font family.
#' @param legend.position legend's position.
#' @param ... unused.
#'
#' @return a object of ggplot2.
#' @export
#'
#' @examples
#' model <- repeated_measures_long(data = ldata,
#'                                 between = treatment,
#'                                 within = time,
#'                                 response = score,
#'                                 subject = subject)
#'
#' gg_plot(model, error.type = "se")
#' gg_plot(model, error.type = "ci")
#' gg_plot(model, error.type = "none")
gg_plot <- function(model,
                    error.type = c("se", "ci", "sd", "none"),
                    method = "revpairwise",
                    adjust = "bonferroni",
                    ref = 1,
                    signif = TRUE,
                    error.width = 0.1,
                    line.size = 0.25,
                    point.size = 1.5,
                    vjust.signif = -0.5,
                    family = "serif",
                    legend.position = NULL,
                    ...){

  error.type <- match.arg(error.type)
  between    <- names(attr(model, "between"))
  within     <- names(attr(model, "within"))

  dat <- plotdata(model, error = error.type)

  plot <- ggplot2::ggplot() +
    ggplot2::geom_point(data = dat, ggplot2::aes_string(x= within, y = "emmean", group = between, color = between), size = point.size) +
    ggplot2::geom_line (data = dat, ggplot2::aes_string(x= within, y = "emmean", group = between, color = between), size = line.size)

  if(error.type != "none"){
    plot <- plot +
      ggplot2::geom_errorbar(data = dat,
                             ggplot2::aes_string(x= within, y = "emmean", group = between, color = between, ymin = "lower", ymax = "upper"),
                             width = error.width, size = line.size)

    ybreaks <- pretty(c(0, dat[["upper"]]))
  }else{
    ybreaks <- pretty(c(0, dat[["emmean"]]))
  }

  if(signif){
    y.pos <- dat %>%
      dplyr::select(1, 2, 5) %>%
      dplyr::group_by(!!as.name(within)) %>%
      dplyr::summarise(y.pos = max(.data$upper))

    data.signif <-
      between_effect_contrast(model, type = "simple", method = method, adjust = adjust, ref = ref) %>%
      dplyr::select(within, .data$star) %>%
      dplyr::left_join(y.pos, by = within)

    plot <- plot +
      ggplot2::geom_text(data = data.signif,
                         ggplot2::aes_string(x = within, y = "y.pos", label = "star"),
                         vjust = vjust.signif, family = family)
  }

  plot <- plot +
    theme_sci(line.size = line.size, font.family = family, ...) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::scale_y_continuous(breaks = ybreaks, limits = c(min(ybreaks), max(ybreaks)), expand = c(0, 0)) +
    ggplot2::scale_x_discrete(expand = c(0.15, 0.15)) +
    ggplot2::ylab("Least-squares means")

  if(!is.null(legend.position)){
    plot <- plot + legend_position(legend.position)
  }

  plot
}


plotdata <- function(model, error = "se"){

  response <- attr(model, "dv")
  between  <- names(attr(model, "between"))
  within   <- names(attr(model, "within"))

  ems <- emmeans::emmeans(model, specs = between, by = within)
  dat <- summary(ems)
  dat <- tibble::as_tibble(dat)

  if(error== "se"){
    dat <- dat %>%
      dplyr::select(1:4) %>%
      dplyr::mutate(lower = .data$emmean - .data$SE) %>%
      dplyr::mutate(upper = .data$emmean + .data$SE) %>%
      dplyr::select(-.data$SE)
  }else if (error == "ci"){
    dat <- dat %>%
      dplyr::select(1:3, 6:7) %>%
      dplyr::rename(lower = "lower.CL", upper = "upper.CL")
  }else if(error == "sd"){
    dat <- group_exec(model$data$long, group = c(between, within), func = function(d){
      tibble::tibble(emmean = mean(d[[response]]),
                     lower  = mean(d[[response]]) - stats::sd(d[[response]]),
                     upper  = mean(d[[response]]) + stats::sd(d[[response]]))
    })
  }else{
    dat <- dat %>%
      dplyr::select(1:3) %>%
      dplyr::mutate(lower = .data$emmean) %>%
      dplyr::mutate(upper = .data$emmean)
  }
  dat
}
