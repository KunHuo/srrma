
rm_results <- function(model, digits.pvalue = 3){
  smod <- summary(model)

  univariate.tests <- smod$univariate.tests[-1, 5:6]


  if(smod$sphericity.tests[1, 2] < 0.05){
    univariate.tests[2, 2] <- smod$pval.adjustments[1, 2]
    univariate.tests[3, 2] <- smod$pval.adjustments[2, 2]
  }

  univariate.tests <- as.data.frame(univariate.tests)
  univariate.tests <- tibble::add_column(univariate.tests, effect = c("group", "time", "interaction"), .before = 1)

  univariate.tests
}
