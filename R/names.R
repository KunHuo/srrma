string_pvalue <- function(language = "en", superscript = ""){
  if(language == "en"){
    res <- "P value"
  }else{
    res <- "\u0050\u503c"
  }
  trimws(paste(res, superscript,sep = " "))
}


string_pvalue_code <- function(language = "en", superscript = ""){
  if(language == "en"){
    res <- "*P<0.05, **P<0.01, ***P<0.001."
  }else{
    res <- "*P<0.05\uff0c**P<0.01\uff0c***P<0.001\u3002"
  }
  trimws(paste(res, superscript,sep = " "))
}


string_difference <- function(language = "en", superscript = ""){
  if(language == "en"){
    res <- "Between-group difference"
  }else{
    res <- "\u7ec4\u95f4\u5dee\u5f02"
  }
  trimws(paste(res, superscript,sep = " "))
}


string_effect <- function(language = "en", superscript = ""){
  if(language == "en"){
    res <- "Effect"
  }else{
    res <- "\u6548\u5e94"
  }
  trimws(paste(res, superscript,sep = " "))
}


string_main_effect <- function(language = "en", superscript = ""){
  if(language == "en"){
    res <- "Main effect"
  }else{
    res <- "\u4e3b\u6548\u5e94"
  }
  trimws(paste(res, superscript,sep = " "))
}


string_simple_effect <- function(language = "en", superscript = ""){
  if(language == "en"){
    res <- "Simple effect"
  }else{
    res <- "\u5355\u72ec\u6548\u5e94"
  }
  trimws(paste(res, superscript,sep = " "))
}


string_group_effect_title <- function(language = "en", superscript = ""){
  if(language == "en"){
    res <- "Between-group effects for repeated measures ANOVA"
  }else{
    res <- "\u91cd\u590d\u6d4b\u91cf\u65b9\u5dee\u5206\u6790\u7684\u7ec4\u95f4\u6548\u5e94"
  }
  trimws(paste(res, superscript,sep = " "))
}


string_data_prensent_group_effect <- function(language = "en", se = TRUE, superscript = ""){
  if(language == "en"){
    if(se){
      res <- "Data are prensent as least-squares means (SE), unless otherwise spicified."
    }else{
      res <- "Data are prensent as least-squares means (CI), unless otherwise spicified."
    }
  }else{
    if(se){
      res <- "\u9664\u51fa\u7279\u522b\u6307\u660e\u5916\uff0c\u6570\u636e\u4ee5\u6700\u5c0f\u4e8c\u4e58\u5747\u6570\uff08\u0053\u0045\uff09\u8868\u793a\u3002"
    }else{
      res <- "\u9664\u51fa\u7279\u522b\u6307\u660e\u5916\uff0c\u6570\u636e\u4ee5\u6700\u5c0f\u4e8c\u4e58\u5747\u6570\uff08\u0043\u0049\uff09\u8868\u793a\u3002"
    }
  }
  trimws(paste(superscript,res, sep = " "))
}


string_multiple_comparison <- function(language = "en", adjust, superscript = ""){
  if(language == "en"){
    res <- sprintf("Multiple comparisons were performed using the %s test.", adjust)
  }else{
    res <- sprintf("\u591a\u91cd\u6bd4\u8f83\u91c7\u7528%s\u68c0\u9a8c\u3002", adjust)
  }
  trimws(paste(superscript, res, sep = " "))
}


string_table_number <- function(language, number){
  if(language == "en"){
    sprintf("Table:%s", number)
  }else{
    sprintf("\u8868%s", number)
  }
}


string_contrast <- function(language = "en", superscript = ""){
  if(language == "en"){
    res <- "Contrast"
  }else{
    res <- "\u6bd4\u8f83\u9879"
  }
  trimws(paste(res, superscript,sep = " "))
}

string_time_difference <- function(language = "en", superscript = ""){
  if(language == "en"){
    res <- "Time effect difference"
  }else{
    res <- "\u65f6\u95f4\u6548\u5e94\u5dee\u503c"
  }
  trimws(paste(res, superscript,sep = " "))
}

string_linear_pvalue <- function(language = "en", superscript = ""){
  if(language == "en"){
    res <- "P for linear"
  }else{
    res <- "\u7ebf\u6027\u8d8b\u52bf\u0050\u503c"
  }
  trimws(paste(res, superscript,sep = " "))
}

string_quadratic_pvalue <- function(language = "en", superscript = ""){
  if(language == "en"){
    res <- "P for quadratic"
  }else{
    res <- "\u4e8c\u6b21\u9879\u8d8b\u52bf\u0050\u503c"
  }
  trimws(paste(res, superscript,sep = " "))
}


string_time_effect_title <- function(language = "en", superscript = ""){
  if(language == "en"){
    res <- "Time effects for repeated measures ANOVA"
  }else{
    res <- "\u91cd\u590d\u6d4b\u91cf\u65b9\u5dee\u5206\u6790\u7684\u65f6\u95f4\u6548\u5e94"
  }
  trimws(paste(res, superscript,sep = " "))
}


string_repeated_measures <- function(model, language = "en",superscript = "",  digits.pvalue = 3){
  res <- summary(model)

  pvalues    <- res$univariate.tests[, 6][-1]
  sphericity <- res$sphericity.tests[1, 2]

  if(sphericity < 0.05){
    pvalues[2:3] <- res$pval.adjustments[, 2]
  }

  pvalues <- format_pvalue(pvalues, digits.pvalue)
  index   <- regex_detect(pvalues, pattern = "<|>")
  pvalues[!index] <- paste0("=", pvalues[!index])

if(language == "en"){
  if(sphericity < 0.05){
    out <- sprintf("Univariate Type III Repeated-Measures ANOVA with Greenhouse-Geisser correction, P for group effect %s, P for time effect %s, P for interaction %s.",
                   pvalues[1], pvalues[2], pvalues[3])
  }else{
    out <- sprintf("Univariate Type III Repeated-Measures ANOVA, P for group effect %s, P for time effect %s, P for interaction %s.",
                   pvalues[1], pvalues[2], pvalues[3])
  }
}else if(language == "chn"){
  if(sphericity < 0.05){
    out <- sprintf("\u0047\u0072\u0065\u0065\u006e\u0068\u006f\u0075\u0073\u0065\u002d\u0047\u0065\u0069\u0073\u0073\u0065\u0072\u6821\u6b63\u7684\u5355\u53d8\u91cf\u0020\u0049\u0049\u0049\u0020\u578b\u91cd\u590d\u6d4b\u91cf\u65b9\u5dee\u5206\u6790\uff0c\u0050\u7ec4\u95f4%s\uff0c\u0050\u65f6\u95f4%s\uff0c\u0050\u4ea4\u4e92%s\u3002",
                   pvalues[1], pvalues[2], pvalues[3])
  }else{
    out <- sprintf("\u5355\u53d8\u91cf\u0020\u0049\u0049\u0049\u0020\u578b\u91cd\u590d\u6d4b\u91cf\u65b9\u5dee\u5206\u6790\uff0c\u0050\u7ec4\u95f4%s\uff0c\u0050\u65f6\u95f4%s\uff0c\u0050\u4ea4\u4e92%s\u3002",
                   pvalues[1], pvalues[2], pvalues[3])
  }
}

trimws(paste(superscript, out, sep = " "))
}
