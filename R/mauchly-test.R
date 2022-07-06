mauchly_test <- function(model, digits.pvalue = 3){
  res <- summary(model)$sphericity.tests
  res <- tibble::tibble(within = row.names(res)[1],
                        statistic = format_statistic(res[1, 1], digits.pvalue),
                        p.value = format_pvalue(res[1, 2], digits.pvalue))
  names(res) <- c("Within subjects effect", "Mauchly's W", "P value")

  res <- add_title(res, "Mauchly's test of sphericity")

  class(res) <- c("rmanova", class(res))
  res
}
