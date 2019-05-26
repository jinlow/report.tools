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
#' @param extra_vars extra binary variables to distribute in addition to the \code{bivar}.
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
#'
#' @examples
#' # Create a bivariate of vs by cyl
#' bivar_table(x = mtcars, var = "cyl", bivar = "vs")
#'
#' # add extra variable am
#' bivar_table(x = mtcars, var = "cyl", bivar = "vs", extra_vars = "am")
#'
#' # Use custom predifined cut function
#' bivar_table(x = mtcars, var = "mpg", bivar = "vs", extra_vars = "am", cut_fmt = "quantile")
#'
#' @export
bivar_table <- function(x,
                        var,
                        bivar,
                        extra_vars = NULL,
                        cut_fmt = NULL,
                        na_last = FALSE,
                        decreasing = FALSE) {

  stopifnot(inherits(x, "data.frame"))

  if (inherits(x, "data.table")) {
    x <- x[, c(var, bivar, extra_vars), with = FALSE]
  } else {
    x <- x[, c(var, bivar, extra_vars)]
    setDT(x)
  }

  if (sum(is.na(x[, get(bivar)])) > 0) {
    stop("Missing values present in ", bivar)
  }

  # Apply cut_fmt to var
  fmt_cut_fnc_(x, var, cut_fmt = cut_fmt)

  # Create extra_vars columns
  if (!is.null(extra_vars)) {
    extra_cols <- lapply(extra_vars, function(extra_var) {
      extra_cols_(x, extra_var, var, decreasing, na_last)
    })
  }

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

  # Merge extra columns if they exist
  if (!is.null(extra_vars)) {
    btab <- Reduce(function(...) merge(..., by = var, all = TRUE, sort = FALSE), c(list(btab), extra_cols))
  }

  class(btab) <- c("report.table", class(x))
  btab[]
}

# bivar extra cols function
extra_cols_ <- function(x, extra_var, var, decreasing, na_last) {
  if (x[, sum(is.na(get(extra_var)))] > 0) {
    stop("extra_vars cannot contain missing values.", call. = FALSE)
  }
  extr_var <- x[, .("Sum_b" = sum(get(extra_var)),
                    "Pct_of_b" = sum(get(extra_var))/(x[, sum(get(extra_var))])), by = var]
  setnames(extr_var,
           old = c("Sum_b", "Pct_of_b"),
           new = c(paste0("Sum: ", extra_var),
                   paste0("Pct: ", extra_var)))

  # Add Total column
  tot <- data.table("var" = "Total",
                    "Sum_b" = x[, sum(get(extra_var))],
                    "Pct_of_b" = 1)
  setnames(tot,
           old = c("var", "Sum_b", "Pct_of_b"),
           new = c(var, paste0("Sum: ", extra_var),
                   paste0("Pct: ", extra_var)))
  extr_var <- rbindlist(list(extr_var, tot))
}
