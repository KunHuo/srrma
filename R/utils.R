str_align <- function (x, sep = "\\r"){
  id.na <- is.na(x)
  if (length(grep("\\", sep, fixed = TRUE)) == 0) {
    idx <- !grepl(x = x, pattern = sep, fixed = TRUE)
    x[idx] <- paste(x[idx], sep, sep = "")
  }
  if (sep == "\\c")
    return(str_pad(x, width = max(nchar(x), na.rm = TRUE),
                   pad = " ", adj = "center"))
  x <- str_pad(x, max(nchar(x), na.rm = TRUE))
  if (sep == "\\l")
    return(sub("(^ +)(.+)", "\\2\\1", x))
  if (sep == "\\r")
    return(sub("(.+?)( +$)", "\\2\\1", x))
  bef <- substr(x, 1, str_pos(x, sep, fix = TRUE))
  aft <- substr(x, str_pos(x, sep, fix = TRUE) + 1, nchar(x))
  aft <- substr(aft, 1, max(nchar(str_trim(aft, method = "right"))))
  res <- paste(replace(str_pad(bef, max(nchar(bef), na.rm = TRUE),
                               " ", adj = "right"), is.na(bef), ""),
               replace(str_pad(aft, max(nchar(aft), na.rm = TRUE), " ",
                               adj = "left"), is.na(aft), ""), sep = "")
  res[id.na] <- NA
  if (length(grep("\\", sep, fixed = TRUE)) == 0)
    res[idx] <- gsub(sep, " ", res[idx], fixed = TRUE)
  return(res)
}


str_pad <- function (x, width = NULL, pad = " ", adj = "left") {
  .pad <- function(x, width, pad = " ", adj = "left") {
    if (is.na(x))
      return(NA)
    mto <- match.arg(adj, c("left", "right", "center"))
    free <- max(0, width - nchar(x))
    fill <- substring(paste(rep(pad, ceiling(free/nchar(pad))),
                            collapse = ""), 1, free)
    if (free <= 0)
      x
    else if (mto == "left")
      paste(x, fill, sep = "")
    else if (mto == "right")
      paste(fill, x, sep = "")
    else paste(substring(fill, 1, free%/%2), x, substring(fill,
                                                          1 + free%/%2, free), sep = "")
  }
  if (is.null(width))
    width <- max(nchar(x), na.rm = TRUE)
  lgp <- recycle(x = x, width = width, pad = pad,
                 adj = adj)
  sapply(1:attr(lgp, "maxdim"), function(i) .pad(lgp$x[i],
                                                 lgp$width[i], lgp$pad[i], lgp$adj[i]))
}


recycle <- function (...) {
  lst <- list(...)
  maxdim <- max(lengths(lst))
  res <- lapply(lst, rep, length.out = maxdim)
  attr(res, "maxdim") <- maxdim
  return(res)
}


str_pos <- function (x, pattern, pos = 1, ...) {
  pos <- rep(pos, length.out = length(x))
  x <- substr(x, start = pos, stop = nchar(x))
  i <- as.vector(regexpr(pattern = pattern, text = x, ...))
  i[i < 0] <- NA
  return(i)
}


str_trim <- function (x, pattern = " \t\n", method = "both") {
  switch(
    match.arg(arg = method, choices = c("both", "left", "right")),
    both = {
      gsub(
        pattern = gettextf("^[%s]+|[%s]+$", pattern, pattern),
        replacement = "",
        x = x
      )
    },
    left = {
      gsub(
        pattern = gettextf("^[%s]+", pattern),
        replacement = "",
        x = x
      )
    },
    right = {
      gsub(
        pattern = gettextf("[%s]+$", pattern),
        replacement = "",
        x = x
      )
    }
  )
}


str_trunc <- function (x, maxlen = 20, ellipsis = "...", wbound = FALSE) {
  x[!(valid <- !is.na(x))] <- ""
  maxlen <- rep(maxlen, length.out = length(x))
  if (wbound) {
    for (i in seq_along(x)) {
      if (nchar(x[i]) > maxlen[i]) {
        ll <- gregexpr("\\b\\W+\\b", x[i], perl = TRUE)[[1]]
        j <- ll <= maxlen[i]
        maxlen[i] <- if (all(!j)) {
          maxlen[i]
        }
        else {
          max(ll[ll <= maxlen[i]])
        }
      }
    }
  }
  res <- paste0(substr(x, 0L, maxlen), ifelse(nchar(x) > maxlen, ellipsis, ""))
  res[!valid] <- NA_character_
  return(res)
}


regex_extract <- function(string,
                          pattern,
                          ignore.case = FALSE,
                          perl = FALSE,
                          fixed = FALSE,
                          useBytes = FALSE){
  regmatches(string,
             regexpr(pattern,
                     string,
                     ignore.case = ignore.case,
                     perl = perl,
                     fixed = fixed,
                     useBytes = useBytes))
}


regex_extract_all <- function(string,
                              pattern,
                              ignore.case = FALSE,
                              perl = FALSE,
                              fixed = FALSE,
                              useBytes = FALSE){
  regmatches(string,
             gregexpr(pattern,
                      string,
                      ignore.case = ignore.case,
                      perl = perl,
                      fixed = fixed,
                      useBytes = useBytes))
}


regex_replace <- function(string,
                          pattern,
                          replacement,
                          ignore.case = FALSE,
                          perl = FALSE,
                          fixed = FALSE,
                          useBytes = FALSE){
  sub(pattern = pattern,
      replacement = replacement,
      x = string,
      ignore.case = ignore.case,
      perl = perl,
      fixed = fixed,
      useBytes = useBytes)
}


regex_replace_all <- function(string,
                              pattern,
                              replacement,
                              ignore.case = FALSE,
                              perl = FALSE,
                              fixed = FALSE,
                              useBytes = FALSE){
  gsub(pattern = pattern,
       replacement = replacement,
       x = string,
       ignore.case = ignore.case,
       perl = perl,
       fixed = fixed,
       useBytes = useBytes)
}


regex_locate <- function(string,
                         pattern,
                         ignore.case = FALSE,
                         perl = FALSE,
                         fixed = FALSE,
                         useBytes = FALSE){
  res <- regexpr(pattern,
                 string,
                 ignore.case = ignore.case,
                 perl = perl,
                 fixed = fixed,
                 useBytes = useBytes)
  locate_start_end(res)
}


regex_locate_all <- function(string,
                             pattern,
                             ignore.case = FALSE,
                             perl = FALSE,
                             fixed = FALSE,
                             useBytes = FALSE){
  res <- gregexpr(pattern,
                  string,
                  ignore.case = ignore.case,
                  perl = perl,
                  fixed = fixed,
                  useBytes = useBytes)
  lapply(res, function(x) locate_start_end(x))
}


locate_start_end <- function(data){
  start <- data
  start[start == -1] <- NA
  end <- start + attr(data, "match.length") - 1
  cbind(start, end)
}


regex_detect <- function(string,
                         pattern,
                         ignore.case = FALSE,
                         perl = FALSE,
                         fixed = FALSE,
                         useBytes = FALSE){
  grepl(
    pattern,
    string,
    ignore.case = ignore.case,
    perl = perl,
    fixed = fixed,
    useBytes = useBytes)
}


regex_split <- function(string,
                        pattern,
                        perl = FALSE,
                        fixed = FALSE,
                        useBytes = FALSE){
  strsplit(
    string,
    pattern,
    perl = perl,
    fixed = fixed,
    useBytes = useBytes)
}


format_digits <- function(x, digits){
  sapply(x, function(i){
    if(is.na(i)){
      i
    }else{
      fmt <- sprintf("%%.%df", digits)
      sprintf(fmt = fmt, i)
    }
  })
}


format_statistic <- function(x, digits) {
  fmt <- paste0("%.", digits, "f")
  pVec <- sapply(x, function(i) {
    if (is.na(i)) {
      NA
    } else {
      sprintf(fmt = fmt, i)
    }
  })

  small <- paste0("<0.", paste0(rep("0", digits - 1), collapse = ""), "1")
  pos.small <- grepl("^0\\.0*$", pVec)
  pVec[pos.small] <- small

  return(pVec)
}


format_pvalue <- function(x, digits) {
  fmt  <- paste0("%.", digits, "f")

  pVec <- sapply(x, function(i){
    if(is.na(i)){
      NA
    }else{
      sprintf(fmt = fmt, i)
    }
  })
  smallPString <- paste0("<0.", paste0(rep("0", digits - 1), collapse = ""), "1")
  posAllZeros <- grepl("^0\\.0*$", pVec)

  pVec[posAllZeros]  <- smallPString
  return(pVec)
}


format_signif_start <- function(pvalues, digits = 3){
  if(is.numeric(pvalues)){
    sign <- ifelse(pvalues <= 0.001, "***",
                   ifelse(pvalues <= 0.01, "**",
                          ifelse(pvalues <= 0.05, "*",
                                 ifelse(pvalues <= 0.1, ".", ""))))
    sign <- ifelse(is.na(sign), "", format(sign, justify = "left"))

    pvalues <- format_pvalue(pvalues, digits)
    pvalues <- ifelse(is.na(pvalues), "", format(pvalues, justify = "right"))
    paste(pvalues, sign, sep = " ")
  }else{
    pvalues.numeric <- as.numeric(gsub(pattern = "<|>", replacement = "", x = pvalues))
    sign <- ifelse(pvalues.numeric <= 0.001, "***",
                   ifelse(pvalues.numeric <= 0.01, "**",
                          ifelse(pvalues.numeric <= 0.05, "*",
                                 ifelse(pvalues.numeric <= 0.1, ".", ""))))
    sign <- ifelse(is.na(sign), "", format(sign, justify = "left"))
    pvalues <- ifelse(is.na(pvalues), "", format(pvalues, justify = "right"))
    paste(pvalues, sign, sep = " ")
  }
}


format_variable <- function(data,
                            varnames = names(data),
                            fold = FALSE,
                            space = 4,
                            sep = " vs. ",
                            add.first = NULL,
                            add.last = NULL){
  space <- create_space(space)
  sep <- sprintf("%s", sep)
  execute <-  function(varname){
    label <- varname
    if(!is.null(data)){
      label <- attr(data[[varname]], "label")
      if(is.null(label)){
        label <- varname
      }
    }
    if(is.numeric(data[[varname]])){
      data.frame(term = varname, varname = varname, ref = FALSE, variable = label, stringsAsFactors = FALSE)
    }else if(is.factor(data[[varname]]) | is.character(data[[varname]])){
      levels <- extract_levels(data[[varname]])
      if(fold){
        style_factor1(varname, levels, label, space = space, sep = sep)
      }else{
        style_factor2(varname, levels, label, space = space)
      }
    }
  }

  res <- lapply(varnames, execute)
  res <- do.call(rbind, res)

  if(!is.null(add.first)){
    if(!is.data.frame(add.first)){
      first <- data.frame(term = add.first, varname = add.first, ref = FALSE,
                          variable = add.first, stringsAsFactors = FALSE)
      res <- rbind(first, res)
    }else{
      res <- rbind(add.first, res)
    }
  }

  if(!is.null(add.last)){
    if(!is.data.frame(add.last)){
      last <- data.frame(term = add.last, varname = add.last, ref = FALSE,
                         variable = add.last, stringsAsFactors = FALSE)
      res <- rbind(res, last)
    }else{
      res <- rbind(res, add.last)
    }
  }
  res
}


create_space <- function(n){
  strrep(" ", n)
}


extract_levels <- function(x){
  if(is.character(x)){
    x <- as.factor(x)
  }
  levels(x)
}


style_factor1 <- function(varname, levels, label, space, sep) {
  if (length(levels) <= 2L) {
    variable <- sprintf("%s (%s%s%s)", label, levels[2], sep, levels[1])
    term <- paste0(varname, levels[2])
    data.frame(term = term, varname = varname, ref = FALSE, variable = variable, stringsAsFactors = FALSE)
  } else{
    term <- paste0(varname, levels[-1])
    term <- c(varname, term)
    variable <- paste(levels[-1], levels[1], sep = sep)
    variable <- paste0(space, variable)
    variable <- c(label, variable)
    data.frame(term = term, varname = varname, ref = FALSE, variable = variable, stringsAsFactors = FALSE)
  }
}


style_factor2 <- function(varname, levels, label, space) {
  term <- paste0(varname, levels)
  term <- c(varname, term)
  variable <- paste0(space, levels)
  variable <- c(label, variable)
  ref <- c(FALSE, TRUE, rep(FALSE, length(levels) - 1))
  data.frame(term = term, varname = varname, ref = ref,  variable = variable, stringsAsFactors = FALSE)
}


n_digits <- function(x){
  x <- as.character(x)
  sapply(x, function(i) {
    i <- regex_split(i, pattern = ".", fixed = TRUE)
    i <- unlist(i)
    if (length(i) == 1L) {
      0
    } else{
      nchar(i[2])
    }
  })
}


max_digits <- function(x){
  max(n_digits(x), na.rm = TRUE)
}


max_nchar <- function(x){
  max(sapply(x, nchar), na.rm = TRUE)
}


check_index <- function(data, index){
  tmp <- index >=1 & index <= ncol(data)
  if(!all(tmp)){
    meaasge <- sprintf("Index must be between 1 and %d.", ncol(data))
    stop(meaasge, call. = FALSE)
  }
}


check_name <- function(data, varnames){
  tmp <- varnames %in% names(data)
  if(!all(tmp)){
    tmpname <- varnames[!tmp]
    tmpname <- paste(tmpname, collapse = ", ")
    message <- sprintf("%s are (is) not included in the data frame.", tmpname)
    stop(message, call. = FALSE)
  }
}


check_installed <- function (pkg, message = FALSE) {
  res <- all(sapply(pkg, function(x) isTRUE(requireNamespace(x, quietly = TRUE))))
  if(res){
    return(res)
  }else{
    if(message){
      stop(sprintf("The '%s' package needs to be installed. Run the following
                   code to install: install.packages('%s').", pkg, pkg), call. = FALSE)
    }else{
      return(res)
    }
  }
}


relocate <- function(data, variables, before = NULL, after = NULL) {
  if (is.numeric(variables)) {
    check_index(data, variables)
    to_move <- variables
  } else{
    check_name(data, variables)
    to_move <- sapply(variables, function(x) { which(names(data) == x) })
    names(to_move) <- NULL
  }

  if (!is.null(before) && !is.null(after)) {
    stop("Must supply only one of `.before` and `.after`.")
  } else if (!is.null(before)) {
    if (is.numeric(before)) {
      check_index(data, before)
      where <- before
    } else{
      check_name(data, before)
      where <- which(names(data) == before)
    }
    if (!where %in% to_move) {
      to_move <- c(to_move, where)
    }
  } else if (!is.null(after)) {
    if (is.numeric(after)) {
      check_index(data, after)
      where <- after
    } else{
      check_name(data, after)
      where <- which(names(data) == after)
    }
    if (!where %in% to_move) {
      to_move <- c(where, to_move)
    }
  } else {
    where <- 1L
    if (!where %in% to_move) {
      to_move <- c(to_move, where)
    }
  }

  lhs <- setdiff(seq2(1, where - 1), to_move)
  rhs <- setdiff(seq2(where + 1, ncol(data)), to_move)

  pos <- unique(c(lhs, to_move, rhs))
  out <- data[pos]
  out
}


seq2 <- function(from, to) {
  if (length(from) != 1) {
    stop("`from` must be length one")
  }
  if (length(to) != 1) {
    stop("`to` must be length one")
  }
  if (from > to) {
    integer()
  }
  else {
    seq.int(from, to)
  }
}


is_empty <- function(x){
  if(is.null(x)){
    return(TRUE)
  }else{
    if(is.na(x)){
      return(TRUE)
    }else{
      if(length(x) == 0L){
        return(TRUE)
      }else{
        if(trimws(x) == ""){
          return(TRUE)
        }else{
          return(FALSE)
        }
      }
    }
  }
}


print_booktabs <- function(data, sep = "__", adj = NULL){

  pad_df <- function(data, adj = "left"){
    if(length(adj) != ncol(data)){
      adj <- c(adj,  rep(adj[length(adj)], ncol(data) - length(adj) ))
    }

    data[,] <-  Map(function(x, a){
      str_pad(x, width = max_nchar(x), adj = a)
    }, data, adj)
    data
  }

  print_lines <- function(data, n.title = 1, n.space = 3){
    for(i in 1:nrow(data)){
      row <- data[i, ,drop = TRUE]
      row <- paste(row, collapse = strrep(" ", n.space))
      lines <- strrep("-", nchar(row))
      if(n.title == 1){
        if(i == 1 | i == 2){
          cat(lines, "\n")
        }
      }else{
        if(i == 1 | i == 4){
          cat(lines, "\n")
        }
      }
      cat(row, "\n")
      if(i== nrow(data)){
        cat(lines, "\n")
      }
    }
  }

  if(is.null(adj)){
    adj <- sapply(data, function(x){
      ifelse(is.numeric(x), "center", "left")
    })
  }

  data[, ] <- lapply(data[, ], function(x){
    if(is.numeric(x)){
      fmt <- sprintf("%%.%df", max_digits(x))
      sprintf(fmt, x)
    }else{
      sapply(x, function(i){
        if(is.na(i)){
          ""
        }else{
          as.character(i)
        }
      })
    }
  })

  if(any(regex_detect(names(data), pattern = sep, fixed = TRUE))){
    titles <- names(data)
    titles <- strsplit(names(data), split = sep, fixed = TRUE)
    titles <- do.call(cbind, titles)
    title1 <- titles[1, ]
    title2 <- titles[2, ]

    data <- rbind(title2, data)
    data <- pad_df(data, adj = adj)
    data <- lapply(data, function(x){ c(strrep("-", max_nchar(x)), x) })
    data <- as.data.frame(data)

    cdata <- lapply(unique(title1), function(x){
      colindex <- which(x == title1)
      tmpdata <- data[, colindex, drop = FALSE]
      tmpdata <- apply(tmpdata, 1, paste, collapse = strrep(" ", 3))
      tmpdata[1] <- strrep("-", nchar(tmpdata[1]))
      tmpdata
    })

    names(cdata) <- unique(title1)
    cdata <- as.data.frame(cdata)
    cdata <- rbind(unique(title1), cdata)
    cdata <- pad_df(cdata, adj = c("left", "center"))

    for(i in seq_along(cdata)){
      if(str_trim(cdata[1, i]) == str_trim(cdata[3, i])){
        cdata[2, i] <- cdata[1, i]
        cdata[1, i] <- strrep(" ", nchar(cdata[1, i]))
        cdata[3, i] <- strrep(" ", nchar(cdata[3, i]))
      }
    }

    print_lines(cdata, n.title = 2, n.space = 3)
  }else{
    data <- rbind(names(data), data)
    data <- pad_df(data, adj = adj)
    print_lines(data, n.title = 1, n.space = 3)
  }
}


list_rbind <- function(data,
                       names.as.column = TRUE,
                       collapse.names = FALSE,
                       collapse.one.row = FALSE,
                       varname = "variable"){
  if(class(data) != "list"){
    stop("Data must be a list.", call. = FALSE)
  }

  data <- data[!sapply(data, is.null)]

  NAMES <- names(data)

  if(is.null(NAMES)){
    NAMES <- sprintf("%d", 1:length(data))
  }

  collapse <- function(d, nm){
    d[[1]] <- paste0("    ", d[[1]])
    rbind(c(nm, rep(NA, ncol(d) - 1)), d)
  }

  collapse_column <- function(d, nm){
    if(collapse.one.row){
      collapse(d, nm)
    }else{
      if(nrow(d) == 1L){
        tmpname <- names(d)
        d <- cbind(data.frame(nm), d[, -1, drop = FALSE])
        names(d) <- tmpname
        d
      }else{
        collapse(d, nm)
      }
    }
  }

  out <- Map(function(d, nm){
    if(names.as.column){
      if(collapse.names){
        collapse_column(d, nm)
      }else{
        cbind(data.frame(variable = nm), d)
      }
    }else{
      d
    }
  }, data, NAMES)

  out <- do.call(rbind, out)
  row.names(out) <- NULL
  if(names.as.column){
    names(out)[1] <- varname
  }
  out
}


list_cbind <- function(data){

}


group_exec <- function(data, group, func, ...){
  INDICES <- lapply(group, function(x) {
    data[[x]]
  })
  names(INDICES) <- group
  res <- by(data = data, INDICES = INDICES, FUN = func, ..., simplify = FALSE)
  dn <- dimnames(res)
  d <- dim(res)
  by.res <- lapply(X = seq_along(res), FUN = function(i, res) {
    ii <- i - 1L
    out <- NULL
    for (j in seq_along(dn)) {
      iii <- ii%%d[j] + 1L
      ii <- ii%/%d[j]
      out <- c(out, dn[[j]][iii])
    }
    out
  }, res)


  by.res <- do.call(rbind, by.res)
  by.res <- as.data.frame(by.res)

  by.res <- lapply(by.res, function(x){
    rep(x, each = nrow(res[[1]]))
  })

  by.res <- do.call(cbind, by.res)
  by.res <- as.data.frame(by.res)
  colnames(by.res) <- group

  res <- do.call(rbind, res)
  res <- as.data.frame(res)

  res <- cbind(by.res, res)
  rownames(res) <- NULL
  res <- res[order(match(res[[1]], unique(data[[group[1]]]))), ]
  res
}


group_vars <- function(data, groups = NULL){
  if(is.null(groups)){
    if("grouped_df" %in% class(data)){
      tmp <- attr(data, "groups")
      groups <- setdiff(names(tmp), ".rows")
    }
  }
  groups
}


map_dfr <- function(x, func, group = NULL, ...,
                    collapse.names = NULL,
                    collapse.one.row = FALSE,
                    varname = "variable", transpose = TRUE){

  if(is.null(collapse.names)){
    if(is.null(group)){
      collapse.names <- FALSE
    }else{
      collapse.names <- TRUE
    }
  }

  groups <- group_vars(data = x, groups = group)

  nms <- names(x)
  nms <- setdiff(nms, groups)
  names(nms) <- nms

  out <- lapply(nms, \(nm){
    if(is.null(groups)){
      tryCatch({
        do.call(func, args = list(x[[nm]], ...))
      }, error = function(e) {
        print(e)
        NULL })
    }else{
      group_exec(x, group = groups, \(d){
        do.call(func, args = list(d[[nm]], ...))
      })
    }
  })

  # if(transpose){
  #   out <- lapply(out, function(x){
  #     print(x)
  #     transpose(x)
  #   })
  # }

  out

  # list_rbind(out,
  #            collapse.names = collapse.names,
  #            collapse.one.row = collapse.one.row,
  #            varname = varname)
}



map_dfc <- function(x, func, group = NULL, ...){
  groups <- group_vars(data = x, groups = group)

  nms <- names(x)
  nms <- setdiff(nms, groups)
  names(nms) <- nms

  out <- lapply(nms, \(nm){
    if(is.null(groups)){
      tryCatch({
        do.call(func, args = list(x[[nm]], ...))
      }, error = function(e) {
        print(e)
        NULL })
    }else{
      group_exec(x, group = groups, \(d){
        do.call(func, args = list(d[[nm]], ...))
      })
    }
  })

  out
}



merge_left <- function(x, y, by){
  x$.id <- 1:nrow(x)
  res <- merge(x, y, sort = FALSE, by = by, all.x = TRUE)
  res <- res[order(res$.id), ]
  res[, -which(names(res) == ".id")]
}


extract_terms <- function(x, which = 1){
  if(is.character(which)){
    which(names(x) == which)
  }
  variables <- x[[which]]
  term <- vector(length = length(variables))
  varname <- vector(length = length(variables))
  for(i in seq_along(variables)){
    if(regex_detect(variables[i], pattern = "^\\s")){
      tmp <- regex_extract(rev(variables[1:i]), pattern = "^\\S*")
      tmp <- tmp[tmp != ""][1]
      term[i] <-  paste0(tmp, trimws(variables[i]))
      varname[i] <- tmp
    }else{
      term[i] <- variables[i]
      varname[i] <- variables[i]
    }
  }
  data.frame(term = term, varname = varname)
}


add_terms_column <- function(x, which = 1){
  terms <- extract_terms(x, which = which)
  terms <- terms[, 1, drop = FALSE]
  x.class <- class(x)
  x.title <- attr(x, "title")
  x.note <- attr(x, "note")
  x <- cbind(terms, x)
  class(x) <- x.class
  attr(x, "title") <- x.title
  attr(x, "note") <- x.note
  x
}


add_varnames_column <- function(x, which = 1){
  terms <- extract_terms(x, which = which)
  terms <- terms[, 2, drop = FALSE]
  x.class <- class(x)
  x.title <- attr(x, "title")
  x.note <- attr(x, "note")
  x <- cbind(terms, x)
  class(x) <- x.class
  attr(x, "title") <- x.title
  attr(x, "note") <- x.note
  x
}



merge_table <- function(x, y, name.x = NULL, name.y = NULL, name.x.index = 2, name.y.index = 2, sep = "__"){
  x.class <- class(x)
  x.title <- attr(x, "title")
  x.note <- attr(x, "note")

  if(!is.null(name.x)){
    names(x) <- c(names(x)[1:(name.x.index - 1)],
                  paste(name.x, names(x)[c(name.x.index:length(names(x)))], sep = sep))
  }

  if(!is.null(name.y)){
    names(y) <- c(names(y)[1:(name.y.index - 1)],
                  paste(name.y, names(y)[c(name.y.index:length(names(y)))], sep = sep))
  }

  x <- add_terms_column(x, which = 1)
  y <- add_terms_column(y, which = 1)
  y <- y[, -2, drop = FALSE]
  out <- merge_left(x, y, by = "term")
  out <- out[, -1, drop = FALSE]

  class(out) <- x.class
  attr(out, "title") <- x.title
  attr(out, "note") <- x.note
  out
}


add_title <- function(x, value = NULL){
  attr(x, "title") <- value
  x
}


add_note <- function(x, value = NULL, append = TRUE){
  if(is_empty(value)){
    attr(x, "note") <- NULL
  }else{
    if(append){
      note <- attr(x, "note")
      if(is_empty(note)){
        attr(x, "note") <- value
      }else{
        attr(x, "note") <- paste(note, value, sep = "\n")
      }
    }else{
      attr(x, "note") <- value
    }
  }
  x
}


add_lables <- function(x, value){
  value[, 1] <- lapply(value[, 1], function(v) {
    for (i in seq_along(v)) {
      if (i != 1) {
        if (is.na(v[i])) {
          v[i] <- v[i - 1]
        }
      }
    }
    v
  })

  terms <- extract_terms(x)
  for(i in 1:nrow(x)){
    term <- terms[i, 1]
    if(term %in% value[[1]]){
      index <- which(term == value[[1]])
      code  <- value[index, 2, drop = TRUE]
      label <- value[index, 3, drop = TRUE]

      if(!is_empty(label)){
        if(is_empty(code)){
          x[i, 1] <- label
        }else{
          x[i, 1] <- regex_replace(string = x[i, 1, drop = TRUE],
                                   pattern = code,
                                   replacement = label)
        }
      }
    }
  }
  x
}


