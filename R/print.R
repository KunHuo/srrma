#' Print 'rmanova' object
#'
#' @param x a object of 'rmanova".
#' @param ... further arguments.
#'
#' @keywords internal
#'
#' @export
print.rmanova <- function(x, ...){
  title <- attr(x, "title")
  notes <- attr(x, "note")

  cat("\n")

  if(!is.null(title)){
    cat(title)
  }

  cat("\n")

  print_booktabs(x, adj = c("left", "center"))

  if(!is.null(notes)){
    cat(notes)
  }

  cat("\n\n")
}


#' Print  object
#'
#' @param x a object of 'srrma".
#' @param ... further arguments.
#'
#' @keywords internal
#'
#' @export
print.srrma<- function(x, ...){
  print(rmanova_effect(x, ...))
}
