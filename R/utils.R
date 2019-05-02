#' @importFrom magrittr "%>%"
NULL

#' @import data.table
NULL

# Check object type
list_ddm <- function(x) {
  types <- unique(purrr::map_chr(x, class))
  all(types %in% c("report.table", "matrix", "data.table", "data.frame"))
}

#' Print report.table
#' S3 print method for objects of class report.table
#' @export
print.report.table <- function(x, ...) {
  w_pct <- grep(pattern = "percent|pct", x = names(x), ignore.case = T)
  xp <- as.data.frame(x)
  xp[,(w_pct)] <- lapply(xp[,(w_pct)] , function(v) {sprintf("%.2f%%", 100*as.numeric(v))})
  print(knitr::kable(xp[], align = ))
}

# Assign appropriate cut function to fmt_cut
# x is a data.table in the function, ... is the
# variable to perform the cut on.
fmt_cut_fnc_ <- function(x, ..., cut_fmt = NULL) {
  vars <- c(...)
  if (is.null(cut_fmt)) {
    NULL
  } else if (is.function(cut_fmt)) {
    x[, (vars) := cut_fmt(get(vars))]
  } else if (cut_fmt == "quantile") {
    x[, (vars) := cut(get(vars),
                     breaks = c(unique(quantile(get(vars), na.rm = TRUE), Inf)),
                     include.lowest = TRUE)]
  } else if (cut_fmt == "decile") {
    x[, (vars) := cut(get(vars),
                     breaks = c(unique(quantile(get(vars),
                                                probs = seq(from = 0,
                                                            to = 1,
                                                            by = 0.1),
                                                na.rm = TRUE), Inf)),
                     include.lowest = TRUE)]
  } else {
    stop(cut_fmt, " is an invalid function for cut_fmt", call. = FALSE)
  }
}
