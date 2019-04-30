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
