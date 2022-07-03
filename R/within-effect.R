#' Report within-subjects effects
#'
#' @param model a object from [repeated_measures_long] or [repeated_measures_wide].
#' @param type a character specifying the effect type, 'main' for main effects,
#' 'simple' for simple effects. 'all' for main and simple effects.
#' @param method a character value giving the root name of a contrast method, see [contrast] or [contrast-methods].
#' @param adjust a character, adjust P-values for multiple comparisons, see [contrast] or [p.adjust].
#' @param ref integer(s) or character(s) specifying which level(s) to use as the
#' reference. Character values must exactly match elements of levs.
#' @param se a logical variable specifying whether to shou standard errors or confidence intervals.
#' @param language language, 'en' for English, 'chn' for Chinese.
#' @param digits.effect digits for effect, default 2.
#' @param digits.pvalue digits for p value, default 3.
#' @param table.number table number.
#'
#' @return a data.frame with class 'rmanova'.
#' @export
#'
#' @examples
#' model <- repeated_measures_long(data = ldata,
#'                                 between = treatment,
#'                                 within = time,
#'                                 response = score,
#'                                 subject = subject)
#'
#' # Main effects and simple effects
#' report_within_effect(model, type = "all")
#'
#' # Main effects
#' report_within_effect(model, type = "main")
#'
#' # Simple effects
#' report_within_effect(model, type = "simple")
#'
#' # Write to docx.
#' # results <- report_within_effect(model, type = "all", table.number = 2)
#' # write_docx(results, path = "Between effect.docx")
report_within_effect <- function(model,
                                 type = c("all", "main", "simple"),
                                 method = "revpairwise",
                                 adjust = "bonferroni",
                                 ref = 1,
                                 se = TRUE,
                                 language = c("en", "chn"),
                                 digits.effect = 2,
                                 digits.pvalue = 3,
                                 table.number = NULL){

  type     <- match.arg(type)
  language <- match.arg(language)

  between  <- names(attr(model, "between"))
  within   <- names(attr(model, "within"))

  contrast <- within_effect_contrast(
    model = model,
    type = type,
    method = method,
    adjust = adjust,
    ref = ref,
    se = se,
    digits.effect = digits.effect,
    digits.pvalue = digits.pvalue)

  contrast <- split.data.frame(contrast, contrast[[between]])

  contrast <- lapply(contrast, function(d){
    d <- d[-1]
    d$star <- NULL
    d$t.ratio <- NULL
    d
  })

  contrast <- list_rbind(contrast, collapse.names = TRUE, collapse.one.row = FALSE, varname = between)

  trend <- emmeans::emmeans(model, specs = within, by = between) %>%
    emmeans::contrast(method = "poly") %>%
    summary() %>%
    dplyr::select(.data$contrast, .data$p.value, between) %>%
    dplyr::bind_rows(
      emmeans::emmeans(model, specs = within) %>%
        emmeans::contrast( method = "poly") %>%
        summary() %>%
        dplyr::select(.data$contrast, .data$p.value) %>%
        dplyr::mutate(!!as.name(between) := "Main effect")) %>%
    dplyr::mutate(p.value = format_pvalue(.data$p.value, digits.pvalue)) %>%
    tidyr::pivot_wider(names_from = "contrast", values_from = "p.value")

  res <- dplyr::left_join(contrast, trend, by = between)

  res <- res %>%
    add_title(string_time_effect_title(language)) %>%
    add_note(string_data_prensent_group_effect(language, se)) %>%
    add_note(string_multiple_comparison(language, adjust, "a"))

  if(!is.null(table.number)){
    title <- attr(res, "title")
    title <- paste(string_table_number(language, table.number), title, sep = "  ")
    attr(res, "title") <- title
  }

  names(res)[names(res) == between]     <- string_contrast(language)
  names(res)[names(res) == "estimate"]  <- string_time_difference(language, "a")
  names(res)[names(res) == "p.value"]   <- string_pvalue(language, "a")
  names(res)[names(res) == "linear"]    <- string_linear_pvalue(language)
  names(res)[names(res) == "quadratic"] <- string_quadratic_pvalue(language)
  res[[1]][res[[1]] == "Main effect"]   <- string_main_effect(language)

  class(res) <- c("rmanova", class(res))
  res
}


within_effect_estimate <- function(model, type = "all", se = TRUE, digits.effect = 2){

  between  <- names(attr(model, "between"))
  within   <- names(attr(model, "within"))

  main.effect <- model %>%
    emmeans::emmeans(specs = within) %>%
    emmean_estimate(se = se, digits.effect = digits.effect) %>%
    tidyr::pivot_wider(names_from = within, values_from = "emmean") %>%
    dplyr::mutate(!!as.name(between) := "Main effect") %>%
    dplyr::relocate(!!as.name(between))

  simp.effect <- model %>%
    emmeans::emmeans(specs = within, by = between) %>%
    emmean_estimate(se = se, digits.effect = digits.effect) %>%
    tidyr::pivot_wider(names_from = within, values_from = "emmean")

  if(type == "all"){
    rbind(simp.effect, main.effect)
  }else if(type == "main"){
    main.effect
  }else if(type == "simple"){
    simp.effect
  }
}


within_effect_contrast <- function(model,
                                   type = "all",
                                   method = "revpairwise",
                                   adjust = "bonferroni",
                                   ref = 1,
                                   se = TRUE,
                                   digits.effect = 2,
                                   digits.pvalue = 3){

  between  <- names(attr(model, "between"))
  within   <- names(attr(model, "within"))

  main.effect <- model %>%
    emmeans::emmeans(specs = within) %>%
    contrast_estimate(method = method,
                      adjust = adjust,
                      ref = ref,
                      se = se,
                      digits.effect = digits.effect,
                      digits.pvalue = digits.pvalue) %>%
    dplyr::mutate(!!as.name(between) := "Main effect") %>%
    dplyr::relocate(!!as.name(between))

  simp.effect <- model %>%
    emmeans::emmeans(specs = within, by = between) %>%
    contrast_estimate(method = method,
                      adjust = adjust,
                      ref = ref,
                      se = se,
                      digits.effect = digits.effect,
                      digits.pvalue = digits.pvalue) %>%
    dplyr::relocate(!!as.name(between))

  if(type == "all"){
    rbind(simp.effect, main.effect)
  }else if(type == "main"){
    main.effect
  }else if(type == "simple"){
    simp.effect
  }
}




