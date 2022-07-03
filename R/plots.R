gg_plot <- function(model){

  response <- attr(model, "dv")
  between  <- names(attr(model, "between"))
  within   <- names(attr(model, "within"))


  plotdata <- group_exec(model$data$long, group = c(between, within), func = function(d){
    tibble::tibble(mean = mean(d[[response]]),
               sd = stats::sd(d[[response]]),
               lower = mean - stats::sd(d[[response]]),
               upper = mean + stats::sd(d[[response]]))
  })

  plotdata[[between]] <- factor(plotdata[[between]])


  plot <- ggplot2::ggplot(plotdata) +
    ggplot2::aes_string( x= within, y = "mean", group = between, color = between) +
    ggplot2::geom_point() +
    ggplot2::geom_line()


  # plot <- plot +
  #   ggplot2::geom_errorbar(ggplot2::aes_string(ymin = "lower", ymax = "upper"), width = 0.12)

  ybreaks <- pretty(c(0, plotdata[["mean"]]))

  plot +
    theme_sci() +
    ggplot2::scale_y_continuous(breaks = ybreaks, limits = c(min(ybreaks), max(ybreaks)), expand = c(0, 0)) +
    ggplot2::scale_x_discrete(expand = c(0.15, 0.15))
}


