#' Effects of Repeated-measures ANOVA
#'
#' @param model a object from [rmanova_long] or [rmanova_wide].
#' @param digits.effect digits for effect, default 2.
#' @param digits.pvalue digits for p value, default 3.
#' @param language language, 'en' for English, 'chn' for Chinese.
#' @param table.number table number.
#'
#' @return a data.frame.
#' @export
#'
#' @examples
#' model <- rmanova_long(data     = ldata,
#'                       between  = treatment,
#'                       within   = time,
#'                       response = score,
#'                       subject  = subject)
#'
#' rmanova_effect(model)
rmanova_effect <- function(model,
                           digits.effect = 2,
                           digits.pvalue = 3,
                           language = "en",
                           table.number = NULL){
  smod <- summary(model)

  res <- smod$univariate.tests[-1, 5:6]

  if(smod$sphericity.tests[1, 2] < 0.05){
    res[2, 2] <- smod$pval.adjustments[1, 2]
    res[3, 2] <- smod$pval.adjustments[2, 2]
  }

  res <- as.data.frame(res)
  res <- tibble::rownames_to_column(res, var = "effect")
  res[[2]] <- format_statistic(res[[2]], digits.pvalue)
  res[[3]] <- format_pvalue(res[[3]], digits.pvalue)
  names(res) <- c("effect", "F", string_pvalue(language))


  res <- effect_size(model, digits = digits.effect) %>%
    dplyr::left_join(res, by = "effect")

  names(res)[1] <- string_effect(language)

  if(!is.null(table.number)){
    title <- attr(res, "title")
    title <- paste(string_table_number(language, table.number), title, sep = "  ")
    attr(res, "title") <- title
  }

  res <- res %>%
    add_title(string_title_effect(language)) %>%
    add_note(string_note_effectsize(language)) %>%
    add_note(string_note_mauchly(language, format_pvalue(smod$sphericity.tests[1, 2], digits.pvalue)))

  if(smod$sphericity.tests[1, 2] < 0.05){
    res <- add_note(res, string_note_correction(language))
  }

  class(res) <- c("rmanova", class(res))
  res
}


effect_size <- function(model, type = c("eta", "epsilon"), digits = 2){
  type <- match.arg(type)

  res <- effectsize::effectsize(model, type = type)
  res[[2]] <- sprintf("%s (%s, %s)",
                      format_digits(res[[2]], digits),
                      format_digits(res[[4]], digits),
                      format_digits(res[[5]], digits))
  res <- res[, 1:2]
  if(type == "eta"){
    names(res) <- c("effect", "Eta2 (95% CI)")
  }else if(type == "epsilon"){
    names(res) <- c("effect", "Epsilon2 (95% CI)")
  }
  res
}


string_title_effect <- function(language = "en"){
  if(language == "en"){
    "Univariate Type III Repeated-Measures ANOVA"
  }else{
    "\u5355\u53d8\u91cf\u0020\u0049\u0049\u0049\u0020\u578b\u91cd\u590d\u6d4b\u91cf\u65b9\u5dee\u5206\u6790"
  }
}


string_note_effectsize <- function(language){
  if(language == "en"){
    "Partial generalized eta-square is used as a measure of effect size."
  }else{
    "\u91c7\u7528\u90e8\u5206\u5e7f\u4e49\u0065\u0074\u0061\u0032\u8861\u91cf\u6548\u5e94\u503c\u5927\u5c0f\u3002"
  }
}

string_note_correction <- function(language){
  if(language == "en"){
    "Sphericity correction using Greenhouse-Geisse (GG) method."
  }else{
    "\u91c7\u7528\u0020\u0047\u0072\u0065\u0065\u006e\u0068\u006f\u0075\u0073\u0065\u002d\u0047\u0065\u0069\u0073\u0073\u0065\u0020\u0028\u0047\u0047\u0029\u0020\u6cd5\u8fdb\u884c\u7403\u5f62\u5ea6\u6821\u6b63\u3002"
  }
}


string_note_mauchly <- function(language, p.value){
  if(language == "en"){
    sprintf("P value for the Mauchly's sphericity test is %s.", p.value)
  }else{
    sprintf("\u004d\u0061\u0075\u0063\u0068\u006c\u0079\u0020\u7403\u5f62\u5ea6\u68c0\u9a8c\u7684\u0050\u503c\u4e3a%s\u3002", p.value)
  }
}
