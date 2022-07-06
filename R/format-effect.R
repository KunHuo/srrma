emmean_estimate <- function(ems, se = TRUE, digits.effect = 2){
  res <- summary(ems)
  res <- tibble::as_tibble(res)

  if(se){
    res$emmean <- sprintf("%s (%s)",
                          format_digits(res$emmean, digits.effect),
                          format_digits(res$SE, digits.effect))
  }else{
    res$emmean <- sprintf("%s (%s to %s)",
                          format_digits(res$emmean, digits.effect),
                          format_digits(res$lower.CL, digits.effect),
                          format_digits(res$upper.CL, digits.effect))
  }
  res$SE <- NULL
  res$df <- NULL
  res$lower.CL <- NULL
  res$upper.CL <- NULL
  res[,] <- lapply(res, as.character)
  res
}


contrast_estimate <- function(ems,
                              method = "revpairwise",
                              adjust = "bonferroni",
                              ref = 1,
                              multiple.indicators = FALSE,
                              se = TRUE,
                              digits.effect = 2,
                              digits.pvalue = 3){

  results <- emmeans::contrast(ems, method = method, adjust = adjust, ref = ref)
  results <- summary(results)

  #print(str(results))
  results <- tibble::as_tibble(results)

  if(se){
    results$estimate <- sprintf("%s (%s)",
                                format_digits(results$estimate, digits.effect),
                                format_digits(results$SE, digits.effect))
  }else{
    results$estimate <- sprintf("%s (%s to %s)",
                                format_digits(results$estimate, digits.effect),
                                format_digits(results$estimate - 1.96 * results$SE, digits.effect),
                                format_digits(results$estimate + 1.96 * results$SE, digits.effect))
  }

  if(multiple.indicators){
    results$p.value <- stats::p.adjust(results$p.value, adjust)
  }

  results$star <-  ifelse(results$p.value <= 0.001, "***",
                                   ifelse(results$p.value <= 0.01, "**",
                                          ifelse(results$p.value <= 0.05, "*", "")))
  results$p.value <- sprintf("%s", format_pvalue(results$p.value, digits.pvalue))
  results$t.ratio <- sprintf("%s", format_statistic(results$t.ratio, digits.pvalue))
  results$SE <- NULL
  results$df <- NULL
  results[, ] <- lapply(results, as.character)
  results$contrast <- regex_replace(results$contrast, pattern = "-", replacement = "vs.", fixed = TRUE)

  results
}










