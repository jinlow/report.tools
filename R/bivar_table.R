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
#' @param bivar a binary performance vector to destribute along \code{var}
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
                        bivar,
                        cut_fmt = NULL,
                        na_last = FALSE,
                        decreasing = FALSE) {

  stopifnot(inherits(x, "data.frame"))

  if (inherits(x, "data.table")) {
    x <- x[, c(var, bivar), with = FALSE]
  } else {
    x <- x[, c(var, bivar)]
    setDT(x)
  }

  if (sum(is.na(x[, get(bivar)])) > 0) {
    stop("Missing values present in ", bivar)
  }

  # Apply cut_fmt to var
  fmt_cut_fnc_(x, var, cut_fmt = cut_fmt)


  btab <- x[, .("N" = .N,
                "Pct N" = .N/nrow(x),
                "Sum_b" = sum(get(bivar)),
                "Pct_of_b" = sum(get(bivar))/(x[, sum(get(bivar))]),
                "Rate_b" = mean(get(bivar))), by = var]
  setnames(btab,
           old = c("Sum_b", "Pct_of_b", "Rate_b"),
           new = c(paste0("Sum: ", bivar), paste0("Pct: ", bivar), paste0("Rate: ", bivar)))

  if (decreasing) {
    decreasing <- -1L
  } else {
    decreasing <- 1L
  }

  setorderv(btab, cols = var, order = decreasing, na.last = na_last)

  # Add Total column
  tot <- data.table("var" = "Total",
                    "N" = nrow(x),
                    "Pct N" = 1,
                    "Sum_b" = x[, sum(get(bivar))],
                    "Pct_of_b" = 1,
                    "Rate_b" = x[, mean(get(bivar))])
  setnames(tot,
           old = c("var", "Sum_b", "Pct_of_b", "Rate_b"),
           new = c(var, paste0("Sum: ", bivar), paste0("Pct: ", bivar), paste0("Rate: ", bivar)))
  btab <- rbindlist(list(btab, tot))

  class(btab) <- c("report.table", class(x))
  btab[]
}
