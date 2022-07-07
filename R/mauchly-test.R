#' Mauchly's test of sphericity
#'
#' @param model a object from [repeated_measures_long] or [repeated_measures_wide].
#' @param digits.pvalue digits for p value, default 3.
#' @param language language, 'en' for English, 'chn' for Chinese.
#' @param table.number table number.
#'
#' @return a data.frame.
#' @export
mauchly_test <- function(model, digits.pvalue = 3, language = "en", table.number = NULL){
  language <- match.arg(language)

  res <- summary(model)$sphericity.tests
  res <- tibble::tibble(within = row.names(res)[1],
                        statistic = format_statistic(res[1, 1], digits.pvalue),
                        p.value = format_pvalue(res[1, 2], digits.pvalue))
  names(res) <- c(string_within_subject_effects(language), "Mauchly's W", string_pvalue(language))

  res <- add_title(res, string_title_mauchly(language))

  if(!is.null(table.number)){
    title <- attr(res, "title")
    title <- paste(string_table_number(language, table.number), title, sep = "  ")
    attr(res, "title") <- title
  }

  class(res) <- c("rmanova", class(res))
  res
}


string_title_mauchly <- function(language = "en",  superscript = ""){
  if(language == "en"){
    res <- "Mauchly's test of sphericity"
  }else{
    res <- "\u004d\u0061\u0075\u0063\u0068\u006c\u0079\u0020\u7403\u5f62\u68c0\u9a8c"
  }
  trimws(paste(res, superscript, sep = " "))
}


string_within_subject_effects <- function(language = "en",  superscript = ""){
  if(language == "en"){
    res <- "Within subjects effect"
  }else{
    res <- "\u7ec4\u5185\u6548\u5e94"
  }
  trimws(paste(res, superscript, sep = " "))
}
