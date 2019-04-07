#' Frequency Table
#'
#'    \code{freq_table} creates a simple frequency table of one or more
#'    columns.
#'
#' @param x a \code{data.frame} or \code{data.table} object.
#' @param ... The columns to use to create the frequency table, all combinations of the unique
#'   levels of the columns will be used to create the rows of the frequenct table.
#' @param cut_fmt a function used to cut a numeric column into descrete levels
#'   to use in the table. This parameters currently only supports a single numeric column.
#'   Default is set to \code{NULL}. \code{cut_fmt} supports the following
#'   predefined functions:
#'   \itemize{
#'      \item Set \code{cut_fmt = "decile"} to cut the numeric column into decile breaks.
#'      \item Set \code{cut_fmt = "quantile"} to cut the numneric column into quantile breaks.
#'   }
#' @param na_last set to \code{TRUE} to have any \code{NA} values that may be present in the
#'   provided columns to be placed in the last row of the frequency table. Default is set
#'   to \code{FALSE}.
#' @param decreasing if set to \code{TRUE} the unique levels of the columns will be presented
#'    in a decreasing order. Default is set to \code{FALSE}.
#' @return A frequency table of the unique levels of the provided columns
#' @export
freq_table <- function(x,
                       ...,
                       cut_fmt = NULL,
                       na_last = FALSE,
                       decreasing = FALSE) {
  if (!is.data.frame(x)) {
    stop("Input x must be of type data.table or data.frame")
  }
  if (!("data.table" %in% class(x))) {
    x <- as.data.table(x[,c(...), drop = FALSE])
  } else {
    freq_cols <- c(...)
    x <- x[, ..freq_cols]
  }
  if ((length(c(...)) > 1 | !is.numeric(x[, get(c(...)[[1]])])) & !is.null(cut_fmt)) {
    stop("cut_fmt is only defined for a single numeric column")
  }

  # Check for user defined functions
  if (is.null(cut_fmt)) {
    NULL
  } else if (is.function(cut_fmt)) {
    x[, (...) := cut_fmt(get(...))]
  } else if (cut_fmt == "quantile") {
    x[, (...) := cut(get(...),
                     breaks = c(unique(quantile(get(...), na.rm = TRUE), Inf)))]
  } else if (cut_fmt == "decile") {
    x[, (...) := cut(get(...),
                     breaks = c(unique(quantile(get(...),
                                                probs = seq(from = 0,
                                                            to = 1,
                                                            by = 0.1),
                                                na.rm = TRUE), Inf)))]
  } else {
    stop(sprintf("%s, is an invalid function for cut_fmt", cut_fmt))
  }

  x <- x[, .("Frequency" = .N, "Percent" = .N/nrow(x)), by = c(...)]

  if (decreasing) {
    decreasing <- -1L
  } else {
    decreasing <- 1L
  }

  # Order table by columns
  setorderv(x, cols = c(...), order = decreasing, na.last = na_last)

  # Add cumulative functions
  x[, `Cumulative Frequency` := cumsum(Frequency)]
  x[, `Cumulative Percent` := cumsum(Percent)]
  class(x) <- c("report.table", class(x))
  x[]
}
