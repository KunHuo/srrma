describe <- function(model, method = "meansd", digits = 2){
  response <- attr(model, "dv")
  between  <- names(attr(model, "between"))
  within   <- names(attr(model, "within"))

  ldata <- model$data$long

  mean_sd <- function(x){
    sprintf("%s\u00b1%s",
            format_digits(mean(x), digits = digits),
            format_digits(stats::sd(x), digits = digits))
  }

  res <- group_exec(ldata, group = c(within, between), func = function(d){
    data.frame(meansd = mean_sd(d[[response]]))
  })

  tidyr::pivot_wider(res, names_from = between, values_from = "meansd")
}



