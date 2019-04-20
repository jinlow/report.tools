#' Formatted Table
#'
#' @export
formatted_table <- function(x, ...) {
  stopifnot(inherits(x,"data.frame"))
  v_nms <- substitute(c(...))[-1]
  v_nms <- vapply(v_nms, deparse, FUN.VALUE = character(1))
  if (inherits(x,  "data.table")) {
    vars <- x[, (v_nms), with = FALSE]
  } else {
    vars <- x[v_nms]
  }
  x <- as.data.frame.matrix(table(vars))
  x <- data.table::as.data.table(x, keep.rownames = "")
  names(x)[[1]] <- paste(v_nms, collapse = " X ")
  x[]
}
