#' Simple Cross Table
#'
#' Create a table similar to the \code{base} \code{R} function
#' \code{\link[base]{table}}, but instead with formatted column
#' names and as a \code{data.table} so that it can easily be
#' manipulated or put into an analysis.
#'
#' @param x a \code{data.frame} from which to get the columns for
#'    calculating the table.
#'
#' @param ... one or two columns to use to calculate a simple table, or
#'    cross table. Enter the names unquoted.
#'
#' @param var_names custom variable names may be specified and will be used
#'    in the table title. Default \code{NULL}.
#'
#' @param cross_symbol symbol to use in the table title to signify the cross
#'    of two variables. Default is " X ".
#'
#' @examples
#' # Create a cross table of cyl and vs
#' formatted_table(mtcars, cyl, vs)
#'
#' # Add custom names using the var_names parameter
#' formatted_table(mtcars, cyl, vs, var_names = c("Cylinders", "Engine"))
#'
#' # Add a custom by option
#' formatted_table(mtcars, cyl, vs, cross_symbol = " by ")
#'
#' # Allows for functions to be used directly as variable inputs.
#' formatted_table(mtcars, cyl > 6, vs, var_names = c("cyl > 6", "vs"))
#'
#' @export
formatted_table <- function(x, ..., var_names = NULL, cross_symbol = " X ") {
  if (is.null(var_names)) {
    nms <- vapply(substitute(list(...))[-1], deparse, FUN.VALUE = character(1))
  } else {
    nms <- var_names
  }
  # Get columns from data enviroment
  sub_obj <- as.list(substitute(list(...)))[-1L]
  .dat <- lapply(sub_obj, eval, envir = x, enclos = parent.frame())
  if (length(.dat) == 1) {
    tab <- as.data.frame.table(table(unlist(.dat)))
    data.table::setDT(tab)
    names(tab)[[1]] <- nms
  } else if (length(.dat) == 2) {
    tab <- as.data.frame.matrix(table(.dat))
    nms <- paste(nms, collapse = cross_symbol)
    data.table::setDT(tab, keep.rownames = nms)
  } else {
    stop("Currently no more than two variables supported in ...")
  }
  return(tab[])
}

