#' Between-subjects effects
#'
#' @param model a object from [rmanova_long] or [rmanova_wide].
#' @param type a character specifying the effect type, 'main' for main effects,
#' 'simple' for simple effects. 'all' for main and simple effects.
#' @param method a character value giving the root name of a contrast method.
#' @param adjust a character, adjust P-values for multiple comparisons, see [p.adjust].
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
#' model <- rmanova_long(data     = ldata,
#'                       between  = treatment,
#'                       within   = time,
#'                       response = score,
#'                       subject  = subject)
#'
#' # Main effects and simple effects
#' between_effect(model, type = "all")
#'
#' # Main effects
#' between_effect(model, type = "main")
#'
#' # Simple effects
#' between_effect(model, type = "simple")
#'
#' # Write to docx.
#' # results <- between_effect(model, type = "all", table.number = 1)
#' # write_docx(results, path = "Between effect.docx")
between_effect <- function(model,
                           type = c("all", "main", "simple"),
                           method = c("revpairwise", "pairwise", "dunnett", "trt.vs.ctrl"),
                           adjust = c("bonferroni", "holm", "hochberg", "hommel", "BH", "BY", "fdr", "none"),
                           ref = 1,
                           se = TRUE,
                           language = c("en", "chn"),
                           digits.effect = 2,
                           digits.pvalue = 3,
                           table.number = NULL){

  type     <- match.arg(type)
  language <- match.arg(language)
  adjust   <- match.arg(adjust)
  method   <- match.arg(method)

  between <- names(attr(model, "between"))

  args <- list(
    model,
    type = type,
    method = method,
    adjust = adjust,
    ref = ref,
    se = se,
    language = language,
    digits.effect = digits.effect,
    digits.pvalue = digits.pvalue
  )

  if(length(unique(model$data$long[[between]])) == 2L) {
    res <-do.call(cbind_estimate_contrast_2, args = args)
  } else{
    res <-do.call(cbind_estimate_contrast_3, args = args)
  }

  group.size <- model$data$wide %>%
    dplyr::group_by(!!as.name(between)) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::pull(2)

  names(res)[2:(2 + length(group.size) - 1)] <-
    sprintf("%s (n=%d)", names(res)[2:(2 + length(group.size) - 1)], group.size)

  if(!is.null(table.number)){
    title <- attr(res, "title")
    title <- paste(string_table_number(language, table.number), title, sep = "  ")
    attr(res, "title") <- title
  }

  class(res) <- c("rmanova", class(res))
  res
}


cbind_estimate_contrast_2 <- function(model,
                                      type = "all",
                                      method = "revpairwise",
                                      adjust = "bonferroni",
                                      ref = 1,
                                      se = TRUE,
                                      language = "en",
                                      digits.effect = 2,
                                      digits.pvalue = 3) {

  between <- names(attr(model, "between"))
  within  <- names(attr(model, "within"))

  estimate <- between_effect_estimate(
    model = model,
    type = type,
    se = se,
    digits.effect = digits.effect
  )

  contrast <- between_effect_contrast(
    model = model,
    type = type,
    method = method,
    adjust = adjust,
    multiple.indicators = TRUE,
    ref = ref,
    se = se,
    digits.effect = digits.effect,
    digits.pvalue = digits.pvalue
  )

  res <- dplyr::left_join(estimate, contrast[, c(1, 3, 5)], by = within)
  names(res)[1] <- "effect"

  if(type == "all"){
    res[[1]][1:(nrow(res) - 1)] <- paste0("    ", res[[1]][1:(nrow(res) - 1)])
    res <- tibble::add_row(res, effect = string_simple_effect(language), .before = 1)
  }else if(type == "simple"){
    res[[1]][1:nrow(res)] <- paste0("    ", res[[1]][1:nrow(res)])
    res <- tibble::add_row(res, effect = string_simple_effect(language), .before = 1)
  }

  res <- res %>%
    add_title(string_group_effect_title(language, superscript = "a")) %>%
    add_note(string_data_prensent_group_effect(language, se)) %>%
    add_note(string_repeated_measures(model, language, "a", digits.pvalue))

  names(res)[1]                         <- string_effect(language)
  res[[1]][res[[1]] == "Main effect"]   <- string_main_effect(language)
  res[[1]][res[[1]] == "Simple effect"] <- string_simple_effect(language)
  names(res)[names(res) == "estimate"]  <- string_difference(language)
  names(res)[names(res) == "p.value"]   <- string_pvalue(language)

  res
}


cbind_estimate_contrast_3 <- function(model,
                                      type = "all",
                                      method = "revpairwise",
                                      adjust = "bonferroni",
                                      ref = ref,
                                      se = TRUE,
                                      language = "en",
                                      digits.effect = 2,
                                      digits.pvalue = 3) {

  between <- names(attr(model, "between"))
  within  <- names(attr(model, "within"))

  estimate <- between_effect_estimate(
    model = model,
    type = type,
    se = se,
    digits.effect = digits.effect
  )

  contrast <- between_effect_contrast(
      model = model,
      type = type,
      method = method,
      adjust = adjust,
      ref = ref,
      se = se,
      digits.effect = digits.effect,
      digits.pvalue = digits.pvalue) %>%
    dplyr::mutate(contrast = paste(string_difference(language, "b"), .data$contrast, sep = "__")) %>%
    dplyr::mutate(estimate = paste(.data$estimate, .data$star, sep = " ")) %>%
    dplyr::select(1:3) %>%
    tidyr::pivot_wider(names_from = "contrast", values_from = "estimate")

  res <- dplyr::left_join(estimate, contrast, by = within)

  res <- res %>%
    add_note(string_data_prensent_group_effect(language, se))
  names(res)[1] <- "effect"

  if(type == "all"){
    res[[1]][1:(nrow(res) - 1)] <- paste0("    ", res[[1]][1:(nrow(res) - 1)])
    res <- tibble::add_row(res, effect = string_simple_effect(language), .before = 1)
  }else if(type == "simple"){
    res[[1]][1:nrow(res)] <- paste0("    ", res[[1]][1:nrow(res)])
    res <- tibble::add_row(res, effect = string_simple_effect(language), .before = 1)
  }

  names(res)[1]                         <- string_effect(language)
  res[[1]][res[[1]] == "Main effect"]   <- string_main_effect(language)
  res[[1]][res[[1]] == "Simple effect"] <- string_simple_effect(language)

  res %>%
    add_title(string_group_effect_title(language, "a")) %>%
    add_note(string_repeated_measures(model, language, "a", digits.pvalue)) %>%
    add_note(string_multiple_comparison(language, adjust, "b")) %>%
    add_note(string_pvalue_code(language))
}


between_effect_estimate <- function(model, type = "all", se = TRUE, digits.effect = 2){

  between  <- names(attr(model, "between"))
  within   <- names(attr(model, "within"))

  main.effect <- model %>%
    emmeans::emmeans(specs = between) %>%
    emmean_estimate(se = se, digits.effect = digits.effect) %>%
    tidyr::pivot_wider(names_from = between, values_from = "emmean") %>%
    dplyr::mutate(!!as.name(within) := "Main effect") %>%
    dplyr::relocate(!!as.name(within))

  simp.effect <- model %>%
    emmeans::emmeans(specs = between, by = within) %>%
    emmean_estimate(se = se, digits.effect = digits.effect) %>%
    tidyr::pivot_wider(names_from = between, values_from = "emmean")


  if(type == "all"){
    rbind(simp.effect, main.effect)
  }else if(type == "main"){
    main.effect
  }else if(type == "simple"){
    simp.effect
  }
}


between_effect_contrast <- function(model,
                                    type = "all",
                                    method = "revpairwise",
                                    adjust = "bonferroni",
                                    ref = 1,
                                    multiple.indicators = FALSE,
                                    se = TRUE,
                                    digits.effect = 2,
                                    digits.pvalue = 3){

  between  <- names(attr(model, "between"))
  within   <- names(attr(model, "within"))

  main.effect <- model %>%
    emmeans::emmeans(specs = between) %>%
    contrast_estimate(method = method,
                      adjust = adjust,
                      ref = ref,
                      se = se,
                      digits.effect = digits.effect,
                      digits.pvalue = digits.pvalue) %>%
    dplyr::mutate(!!as.name(within) := "Main effect") %>%
    dplyr::relocate(!!as.name(within))

  simp.effect <- model %>%
    emmeans::emmeans(specs = between, by = within) %>%
    contrast_estimate(method = method,
                      adjust = adjust,
                      ref = ref,
                      multiple.indicators = multiple.indicators,
                      se = se,
                      digits.effect = digits.effect,
                      digits.pvalue = digits.pvalue) %>%
    dplyr::relocate(!!as.name(within))

  if(type == "all"){
    rbind(simp.effect, main.effect)
  }else if(type == "main"){
    main.effect
  }else if(type == "simple"){
    simp.effect
  }
}
