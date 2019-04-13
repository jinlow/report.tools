# Internal function for writing tables to excel
table_output <- function(x,
                         wb = NULL,
                         sheet = NULL,
                         start_row = 1,
                         start_col = 1,
                         header_style = NULL,
                         fmt_side = TRUE,
                         pct_keys = "percent|pct|%|rate",
                         keep_na = FALSE) {
  # Set defaul styles
  if (is.null(header_style)) {
    header_style <- openxlsx::createStyle(fgFill = "#4F81BD",
                                          halign = "CENTER",
                                          textDecoration = "Bold",
                                          border = "Bottom",
                                          fontColour = "white")
  }

  # Write out base table
  openxlsx::writeData(wb = wb,
                      sheet = sheet,
                      x = x,
                      startCol = start_col,
                      startRow = start_row,
                      borders = "all",
                      headerStyle = header_style,
                      keepNA = FALSE)

  # Format percent columns
  if (!is.null(pct_keys)) {
    pct_style <- openxlsx::createStyle(numFmt = "PERCENTAGE",
                                       border = "TopBottomLeftRight")
    pct_cols <- grep(pattern = pct_keys, x = names(x), ignore.case = TRUE)
    openxlsx::addStyle(wb = wb,
                       sheet = sheet,
                       style = pct_style,
                       rows = (start_row + 1):(nrow(x) + start_row),
                       cols = (pct_cols + (start_col - 1)),
                       gridExpand = TRUE)
  }

  if (fmt_side) {
    openxlsx::addStyle(wb = wb,
                       sheet = sheet,
                       style = header_style,
                       rows = start_row:(nrow(x) + start_row),
                       cols = start_col,
                       gridExpand = TRUE)
  }
}


#' Tables to excel
#'
#'    Write out data in table format easily to excel
#'
#' @param x an list of or single object of class \code{data.frame}, \code{data.table}
#'   , \code{matrix} or \code{report.table} to write out to excel.
#' @param wb a workbook object created with \code{openxlsx::createWorkbook()}.
#'   If no workbook object is set one will be created in the function.
#' @param sheet the name of a sheet created in the workbook object created using
#'   \code{openxlsx::addWorksheet(wb = wb, sheetName = <name of x>)}, where the
#'   name of the sheet will be the name of the object passed into x
#' @param start_row the row to start writing the table or list of tables to.
#' @param start_col the column to start writing the table or list of tables to.
#' @param header_style a style created using \code{openxlsx::createStyle} to apply to
#'   the header of the table
#' @param fmt_side if \code{header_side} should also be applied to the first columns
#'   of the table of list of tables be output. Default is set to \code{TRUE}.
#' @param open_wb if the workbook object should be opened after the table has been
#'   written.
#' @param pct_keys a regex argument to use to search for columns that should be
#'   automatically formatted as percents when the table is output to excel.
#'   Setting this function to \code{NULL} will result in no columns being
#'   formatted as percents. Default is set to \code{"percent|pct|\%|rate"}.
#' @param keep_na if set to \code{FALSE} cells that contain \code{NA} will be
#'   left blank in the excel document. Otherwise "NA" will be written out as
#'   a string in excel. Default is \code{FALSE}.
#' @param rows_btwn if \code{x} is a list of tables the number if rows
#'   to place between tables.
#'
#' @export
output_to_excel <- function(x,
                    wb = NULL,
                    sheet = NULL,
                    start_row = 1,
                    start_col = 1,
                    header_style = NULL,
                    fmt_side = TRUE,
                    open_wb = TRUE,
                    pct_keys = "percent|pct|%|rate",
                    keep_na = FALSE,
                    rows_btwn = 2) {
  # If no wb object provided create one, the decision needs to be made
  # of how the created workbook should be returned...
  if (is.null(wb)) {
    wb <- openxlsx::createWorkbook()
  }

  if (is.null(sheet)) {
    ob_n <- substitute(x)
    sheet <- deparse(ob_n)
    # Check if sheetname already exists in workbook. If it does
    # append 2 onto it.
    if (sheet %in% wb$sheet_names) {
      sheet <- paste0(sheet, "(2)")
    }
    openxlsx::addWorksheet(wb = wb, sheetName = sheet)
  }

  if (any(class(x) %in% c("report.tools", "data.frame", "matrix"))) {
    table_output(x, wb, sheet, start_row, start_col,
                 header_style, fmt_side, pct_keys, keep_na)
  } else if (is.list(x)) {
    if (!list_ddm(x)) {
      stop("Elements of must be of class data.frame, data.table, or matrix")
    }
    for (i in seq_len(length(x))) {
      table_output(x[[i]],
                   wb,
                   sheet,
                   start_row = start_row,
                   start_col,
                   header_style,
                   fmt_side,
                   pct_keys,
                   keep_na)
      start_row <- (start_row + (nrow(x[[i]]) + (rows_btwn+1)))
    }
  }

  if (open_wb) {
    openxlsx::openXL(wb)
  }
}
