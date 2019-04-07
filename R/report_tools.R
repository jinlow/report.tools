#' report.tools: A package of tools for creating analytical reports
#'
#' The report.tools package provides functions for easily creating
#' analytic reports and outputting them to excel.
#'
#' @docType package
#' @name report.tools
NULL

#' @import data.table
NULL

#' S3 print method for objects of class report.table
#' @export
print.report.table <- function(x) {
  w_pct <- grep(pattern = "percent", x = names(x), ignore.case = T)
  xp <- as.data.frame(x)
  xp[,(w_pct)] <- lapply(xp[,(w_pct)] , function(v) {sprintf("%.2f%%", 100*as.numeric(v))})
  print(knitr::kable(xp[], align = ))
}
