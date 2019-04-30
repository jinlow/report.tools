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

#' Bivariate Table
#'
#' Create a table that shows how a binary field distributes along another
#' discrete variable. Supports all unique values in a variable, custom cut
#' functions, or has a limited number of created helper cut functions for
#' continious variables.
#'
#' @param x a \code{data.frame} or \code{data.table} object.
#' @param var a variable to break up the data, either all unique levels will
#'     be used, or binned levels if a \code{cut_fmt} is chosen.
#' @param perf a binary performance vector to destribute along \code{var}
#' @param cut_fmt a function used to cut a numeric column into descrete levels
#'   to use in the table.
#'   Default is set to \code{NULL}. \code{cut_fmt} supports the following
#'   predefined functions:
#'   \itemize{
#'      \item Set \code{cut_fmt = "decile"} to cut the numeric column into decile breaks.
#'      \item Set \code{cut_fmt = "quantile"} to cut the numneric column into quantile breaks.
#'   }
#' @param na_last set to \code{TRUE} to have any \code{NA} values that may be present in the
#'   provided columns to be placed in the last row of the bivariate table. Default is set
#'   to \code{FALSE}.
#' @param decreasing if set to \code{TRUE} the unique levels of the columns will be presented
#'    in a decreasing order. Default is set to \code{FALSE}.
#' @return A bivariate table with the performance field distributed along the levels of the
#'    selected variable.
#' @export
bivar_table <- function(x,
                        var,
                        perf,
                        cut_fmt = NULL,
                        na_last = FALSE,
                        decreasing = FALSE) {

  stopifnot(inherits(x, "data.frame"))

  if (inherits(x, "data.table")) {
    df <- x[, c(var, perf), with = FALSE]
  } else {
    df <- x[, c(var, perf)]
    setDT(df)
  }

  if (sum(is.na(df[, get(perf)])) > 0) {
    stop("Missing values present in ", perf)
  }

  if (is.null(cut_fmt)) {
    NULL
  } else if (is.function(cut_fmt)) {
    df[, (var) := cut_fmt(get(var))]
  } else if (cut_fmt == "quantile") {
    df[, (var) := cut(get(var),
                      breaks = c(unique(quantile(get(var), na.rm = TRUE), Inf)),
                      include.lowest = TRUE)]
  } else if (cut_fmt == "decile") {
    df[, (var) := cut(get(var),
                      breaks = c(unique(quantile(get(var),
                                                 probs = seq(from = 0,
                                                             to = 1,
                                                             by = 0.1),
                                                 na.rm = TRUE), Inf)),
                      include.lowest = TRUE)]
  } else {
    stop(cut_fmt, " is an invalid function for cut_fmt")
  }


  btab <- df[, .("N" = .N,
                 "Pct N" = .N/nrow(df),
                 "Sum_b" = sum(get(perf)),
                 "Pct_of_b" = sum(get(perf))/(df[, sum(get(perf))]),
                 "Rate_b" = mean(get(perf))), by = var]
  setnames(btab,
           old = c("Sum_b", "Pct_of_b", "Rate_b"),
           new = c(paste0("Sum: ", perf), paste0("Pct: ", perf), paste0("Rate: ", perf)))

  if (decreasing) {
    decreasing <- -1L
  } else {
    decreasing <- 1L
  }

  setorderv(btab, cols = var, order = decreasing, na.last = na_last)

  # Add Total column
  tot <- data.table("var" = "Total",
                    "N" = nrow(df),
                    "Pct N" = 1,
                    "Sum_b" = df[, sum(get(perf))],
                    "Pct_of_b" = 1,
                    "Rate_b" = df[, mean(get(perf))])
  setnames(tot,
           old = c("var", "Sum_b", "Pct_of_b", "Rate_b"),
           new = c(var, paste0("Sum: ", perf), paste0("Pct: ", perf), paste0("Rate: ", perf)))
  btab <- rbindlist(list(btab, tot))

  class(btab) <- c("report.table", class(x))
  btab[]
}
