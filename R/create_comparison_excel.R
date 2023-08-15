#' Create Excel report for comparing two data frames
#'
#' Creates a multi-tab report for comparing two data frames with one
#' tab for each element of an object returned by [get_comparison()]
#'
#' @param comparison a list returned from [get_comparison()]
#' @param path a string, path of the Excel file to be written, default is
#'  `"comparison_report.xlsx"` (must have .xlsx suffix)
#'
#' @returns nothing
#' @export
#'
create_comparison_excel <- function (comparison,
                                     path = "comparison_report.xlsx") {

  wb <- openxlsx::createWorkbook()
  options("openxlsx.minWidth" = 6)
  options("openxlsx.maxWidth" = 25)

  # styles
  headerStyle <- openxlsx::createStyle(border = "TopBottomLeftRight",
                                       wrapText = TRUE,
                                       halign = "center",
                                       valign = "center",
                                       textDecoration = "bold",
                                       fgFill = "#b3d9e5")
  generalStyle <- openxlsx::createStyle(border = "TopBottomLeftRight",
                                        wrapText = TRUE,
                                        halign = "center",
                                        valign = "center")
  additionStyle <- openxlsx::createStyle(border = "TopBottomLeftRight",
                                         wrapText = TRUE,
                                         halign = "center",
                                         valign = "center",
                                         fontColour = "#006100",
                                         fgFill = "#C6EFCE",
                                         textDecoration = "bold")
  deletionStyle <- openxlsx::createStyle(border = "TopBottomLeftRight",
                                         wrapText = TRUE,
                                         halign = "center",
                                         valign = "center",
                                         fontColour = "#9C0006",
                                         fgFill = "#FFC7CE",
                                         textDecoration = "bold")

  # summary tab, rows and simple column report
  summary_sname <- "summary"
  openxlsx::addWorksheet(wb, sheetName = summary_sname)
  openxlsx::setColWidths(wb,
                         sheet = summary_sname,
                         cols = 1:ncol(comparison$col_summary_simple),
                         widths = "auto")
  openxlsx::writeData(wb,
                      sheet = summary_sname,
                      comparison$col_summary_simple,
                      startRow = 1,
                      headerStyle = headerStyle)
  openxlsx::addStyle(wb,
                     sheet = summary_sname,
                     style = generalStyle,
                     rows = 2:(nrow(comparison$col_summary_simple) + 1),
                     cols = 1:ncol(comparison$col_summary_simple),
                     gridExpand = TRUE)
  openxlsx::writeData(wb,
                      sheet = summary_sname,
                      comparison$row_summary,
                      startRow = nrow(comparison$col_summary_simple) + 3,
                      headerStyle = headerStyle)
  openxlsx::addStyle(wb,
                     sheet = summary_sname,
                     style = generalStyle,
                     rows = seq(nrow(comparison$col_summary_simple) + 4,
                                nrow(comparison$col_summary_simple) + 3 +
                                  nrow(comparison$row_summary)),
                     cols = 1:ncol(comparison$col_summary_simple),
                     gridExpand = TRUE)

  # by column name summary tab
  summary_col_sname <- "summary by column"
  openxlsx::addWorksheet(wb, sheetName = summary_col_sname)
  openxlsx::setColWidths(wb,
                         sheet = summary_col_sname,
                         cols = 1:ncol(comparison$col_summary_by_col),
                         widths = 15)
  openxlsx::writeData(wb,
                      sheet = summary_col_sname,
                      comparison$col_summary_by_col,
                      headerStyle = headerStyle,
                      withFilter = TRUE)
  openxlsx::freezePane(wb, sheet = summary_col_sname, firstRow = TRUE)
  openxlsx::addStyle(wb,
                     sheet = summary_col_sname,
                     style = generalStyle,
                     rows = 2:(nrow(comparison$col_summary_by_col) + 1),
                     cols = 1:ncol(comparison$col_summary_by_col),
                     gridExpand = TRUE)

  # all other data frames
  y <- names(comparison) %>%
    setdiff(c("col_summary_simple",
              "col_summary_by_col",
              "row_summary",
              "all_index_by_col",
              "change_index_by_col",
              "change_index_by_col_lr",
              "id_cols",
              "cc_out")) %>%
    purrr::map(\(x) {
      openxlsx::addWorksheet(wb, sheetName = x)
      openxlsx::setColWidths(wb,
                             sheet = x,
                             cols = 1:ncol(comparison[[x]]),
                             widths = 15)
      openxlsx::writeData(wb,
                          sheet = x,
                          x = comparison[[x]],
                          headerStyle = headerStyle,
                          withFilter = TRUE)
      freeCols <- colnames(comparison[[x]]) %>%
        setdiff(c("source",
                  comparison$id_cols,
                  "discrepancy",
                  "change group",
                  "exact dup cnt",
                  "exact dup cnt.df1",
                  "exact dup cnt.df2",
                  "ID dup cnt"))
      openxlsx::freezePane(wb,
                           sheet = x,
                           firstActiveRow = 2,
                           firstActiveCol = which(colnames(comparison[[x]]) == freeCols[1]))
      openxlsx::addStyle(wb,
                         sheet = x,
                         style = generalStyle,
                         rows = 2:(nrow(comparison[[x]]) + 1),
                         cols = 1:ncol(comparison[[x]]),
                         gridExpand = TRUE)

      # conditionally apply addition/deletion formatting based on the sheet
      if (x == "adds") {
        openxlsx::addStyle(wb,
                           sheet = x,
                           style = additionStyle,
                           rows = 2:(nrow(comparison[[x]]) + 1),
                           cols = seq(which(colnames(comparison[[x]]) == freeCols[1]),
                                      ncol(comparison[[x]])),
                           gridExpand = TRUE)

      } else if (x == "dels") {
        openxlsx::addStyle(wb,
                           sheet = x,
                           style = deletionStyle,
                           rows = 2:(nrow(comparison[[x]]) + 1),
                           cols = seq(which(colnames(comparison[[x]]) == freeCols[1]),
                                      ncol(comparison[[x]])),
                           gridExpand = TRUE)

      } else if (x %in% c("all", "changed", "changed_lr")) {

        if (x == "all") {

          # addition rows
          openxlsx::addStyle(wb,
                             sheet = x,
                             style = additionStyle,
                             rows = 1 + stringr::str_which(comparison[[x]]$discrepancy, "addition") ,
                             cols = seq(which(colnames(comparison[[x]]) == freeCols[1]),
                                        ncol(comparison[[x]])),
                             gridExpand = TRUE)

          # deletion rows
          openxlsx::addStyle(wb,
                             sheet = x,
                             style = deletionStyle,
                             rows = 1 + stringr::str_which(comparison[[x]]$discrepancy, "deletion") ,
                             cols = seq(which(colnames(comparison[[x]]) == freeCols[1]),
                                        ncol(comparison[[x]])),
                             gridExpand = TRUE)
        }

        # values in change rows
        if (x == "all") {
          index_lookup <- comparison$all_index_by_col
        } else if (x == "changed") {
          index_lookup <- comparison$change_index_by_col
        } else if (x == "changed_lr") {
          index_lookup <- comparison$change_index_by_col_lr
        } else {
          stop("x must be one of `c('all', 'changed', 'change_lr')`")
        }
        for (el in seq_along(index_lookup)) {
          if (all(is.na(index_lookup[[el]]))) { next }
          if (x %in% c("all", "changed")) {
            cols <- which(colnames(comparison[[x]]) == names(index_lookup)[el])
            df1_rows <- 1 + which(comparison[[x]]$source == "df1")
            df2_rows <- 1 + which(comparison[[x]]$source == "df2")
            change_rows <- 1 + index_lookup[[el]]
            add_rows <- intersect(df1_rows, change_rows)
            del_rows <- intersect(df2_rows, change_rows)
            openxlsx::addStyle(wb,
                               sheet = x,
                               style = additionStyle,
                               rows = add_rows,
                               cols = cols,
                               gridExpand = TRUE)
            openxlsx::addStyle(wb,
                               sheet = x,
                               style = deletionStyle,
                               rows = del_rows,
                               cols = cols,
                               gridExpand = TRUE)
          } else if (x == "changed_lr") {
            rows <- 1 + index_lookup[[el]]
            add_cols <- paste0(names(index_lookup)[el], ".df1")
            add_cols <- which(colnames(comparison[[x]]) == add_cols)
            del_cols <- paste0(names(index_lookup)[el], ".df2")
            del_cols <- which(colnames(comparison[[x]]) == del_cols)
            openxlsx::addStyle(wb,
                               sheet = x,
                               style = additionStyle,
                               rows = rows,
                               cols = add_cols,
                               gridExpand = TRUE)
            openxlsx::addStyle(wb,
                               sheet = x,
                               style = deletionStyle,
                               rows = rows,
                               cols = del_cols,
                               gridExpand = TRUE)
          }
        } # end of for loop through index lookup
      } # end of if statement for all, changed, and changed_lr comparison elements
    })

  # save the file
  openxlsx::saveWorkbook(wb,
                         file = path,
                         overwrite = T)

}
