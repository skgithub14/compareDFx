#' Create Excel report for comparing two data frames
#'
#' Creates a multi-tab report for comparing two data frames with one
#' tab for each element of an object returned by [get_comparison()]
#'
#' @param comparison a list returned from [get_comparison()]
#' @param path a string, path of the Excel file to be written, default is
#'  `"comparison_report.xlsx"` (must have .xlsx suffix)
#' @param autoOpen a logical indicating whether the Excel file should be
#'  automatically opened, default is `FALSE`
#'
#' @returns nothing
#' @export
#'
create_comparison_excel <- function (comparison,
                                     path = "comparison_report.xlsx",
                                     autoOpen = FALSE) {

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
  headerLeftStyle <- openxlsx::createStyle(border = "TopBottomLeftRight",
                                           wrapText = TRUE,
                                           halign = "left",
                                           valign = "center",
                                           textDecoration = "bold",
                                           fgFill = "#b3d9e5")
  generalStyle <- openxlsx::createStyle(border = "TopBottomLeftRight",
                                        wrapText = TRUE,
                                        halign = "center",
                                        valign = "center")
  generalGreyStyle <- openxlsx::createStyle(border = "TopBottomLeftRight",
                                            wrapText = TRUE,
                                            halign = "center",
                                            valign = "center",
                                            fgFill = "lightgrey")
  generalLeftStyle <- openxlsx::createStyle(border = "TopBottomLeftRight",
                                            wrapText = TRUE,
                                            halign = "left",
                                            valign = "center")
  additionStyle1 <- openxlsx::createStyle(border = "TopBottomLeftRight",
                                         wrapText = TRUE,
                                         halign = "center",
                                         valign = "center",
                                         fontColour = "limegreen",
                                         # fontColour = "#006100",
                                         # fgFill = "#C6EFCE",
                                         textDecoration = "bold")
  deletionStyle1 <- openxlsx::createStyle(border = "TopBottomLeftRight",
                                         wrapText = TRUE,
                                         halign = "center",
                                         valign = "center",
                                         fontColour = "firebrick1",
                                         # fontColour = "#9C0006",
                                         # fgFill = "#FFC7CE",
                                         textDecoration = "bold")
  additionStyle2 <- openxlsx::createStyle(border = "TopBottomLeftRight",
                                          wrapText = TRUE,
                                          halign = "center",
                                          valign = "center",
                                          fontColour = "limegreen",
                                          # fontColour = "#006100",
                                          fgFill = "lightgrey",
                                          textDecoration = "bold")
  deletionStyle2 <- openxlsx::createStyle(border = "TopBottomLeftRight",
                                          wrapText = TRUE,
                                          halign = "center",
                                          valign = "center",
                                          fontColour = "firebrick1",
                                          # fontColour = "#9C0006",
                                          fgFill = "lightgrey",
                                          textDecoration = "bold")
  falseConditionalFormatting <- openxlsx::createStyle(fontColour = "#9C0006",
                                                      bgFill = "#FFC7CE",
                                                      textDecoration = "bold")

  # summary tab, rows and simple column report
  summary_sname <- "summary"
  openxlsx::addWorksheet(wb, sheetName = summary_sname)
  openxlsx::setColWidths(wb,
                         sheet = summary_sname,
                         cols = 1:ncol(comparison$row_summary),
                         widths = c(15, 5, 5, 65))

  # one sentence summary
  if (all(comparison$all$discrepancy == "matched")) {
    overall <- "Summary: df1 and df2 matched on all records!!!"
    summaryStatementStyle <- openxlsx::createStyle(fontColour = "#006100",
                                                   fgFill = "#C6EFCE",
                                                   textDecoration = "bold",
                                                   fontSize = 16)
  } else {
    overall <- "Summary: df1 and df2 have different records and/or non-unique records"
    summaryStatementStyle <- openxlsx::createStyle(fontColour = "#9C0006",
                                                   fgFill = "#FFC7CE",
                                                   textDecoration = "bold",
                                                   fontSize = 16)
  }
  openxlsx::writeData(wb,
                      sheet = summary_sname,
                      overall,
                      startRow = 1)
  openxlsx::addStyle(wb,
                     sheet = summary_sname,
                     style = summaryStatementStyle,
                     rows = 1,
                     cols = 1:ncol(comparison$row_summary),
                     gridExpand = TRUE)
  openxlsx::mergeCells(wb, sheet = summary_sname, cols = 5, rows = 1)

  # column simple summary
  openxlsx::writeData(wb,
                      sheet = summary_sname,
                      comparison$col_summary_simple,
                      startRow = 3,
                      headerStyle = headerLeftStyle)
  openxlsx::addStyle(wb,
                     sheet = summary_sname,
                     style = generalLeftStyle,
                     rows = 4:(nrow(comparison$col_summary_simple) + 3),
                     cols = 1:ncol(comparison$col_summary_simple),
                     gridExpand = TRUE)

  # row summary
  openxlsx::writeData(wb,
                      sheet = summary_sname,
                      comparison$row_summary,
                      startRow = nrow(comparison$col_summary_simple) + 5,
                      headerStyle = headerLeftStyle)
  openxlsx::addStyle(wb,
                     sheet = summary_sname,
                     style = generalLeftStyle,
                     rows = seq(nrow(comparison$col_summary_simple) + 6,
                                nrow(comparison$col_summary_simple) + 5 +
                                  nrow(comparison$row_summary)),
                     cols = 1:ncol(comparison$row_summary),
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
  openxlsx::conditionalFormatting(
    wb,
    sheet = summary_col_sname,
    row = seq(2, 1 + nrow(comparison$col_summary_by_col)),
    cols = which(colnames(comparison$col_summary_by_col) %in% c("In df1",
                                                                "In df2",
                                                                "Classes match")),
    rule = "FALSE",
    type = "contains",
    style = falseConditionalFormatting
  )

  # all other data frames
  y <- names(comparison) %>%
    setdiff(c("col_summary_simple",
              "col_summary_by_col",
              "row_summary",
              "all_index_by_col",
              "all_index_by_col_lr",
              "change_index_by_col",
              "change_index_by_col_lr",
              "id_cols",
              "cc_out",
              "matched",
              "adds",
              "dels",
              "changed",
              "changed_lr",
              "exact_dups",
              "id_dups",
              "id_NA")) %>%
    purrr::map(~ {

      # set-up the worksheet and write the data
      if (.x == "all") {
        sname = "data top-bottom"
      } else if (.x == "all_lr") {
        sname = "data left-right"
      } else {
        sname = .x
      }

      openxlsx::addWorksheet(wb, sheetName = sname)
      openxlsx::setColWidths(wb,
                             sheet = sname,
                             cols = 1:ncol(comparison[[.x]]),
                             widths = 15)
      openxlsx::writeData(wb,
                          sheet = sname,
                          x = comparison[[.x]],
                          headerStyle = headerStyle,
                          withFilter = TRUE)

      # freeze panes
      freeCols <- colnames(comparison[[.x]]) %>%
        setdiff(c("source",
                  comparison$id_cols,
                  "discrepancy",
                  "change group",
                  "exact dup cnt",
                  "exact dup cnt.df1",
                  "exact dup cnt.df2",
                  "ID dup cnt",
                  "ID dup cnt.df1",
                  "ID dup cnt.df2"))
      openxlsx::freezePane(
        wb,
        sheet = sname,
        firstActiveRow = 2,
        firstActiveCol = which(colnames(comparison[[.x]]) == freeCols[1])
      )

      # add general styling
      openxlsx::addStyle(wb,
                         sheet = sname,
                         style = generalStyle,
                         rows = 2:(nrow(comparison[[.x]]) + 1),
                         cols = 1:ncol(comparison[[.x]]),
                         gridExpand = TRUE)

      # for df2 entries make the background grey
      if (.x == "all") {
        openxlsx::addStyle(wb,
                           sheet = sname,
                           style = generalGreyStyle,
                           rows = 1 + which(comparison[[.x]]$source == "df2"),
                           cols = 1:ncol(comparison[[.x]]),
                           gridExpand = TRUE)

      } else if (.x == "all_lr") {
        openxlsx::addStyle(wb,
                           sheet = sname,
                           style = generalGreyStyle,
                           rows = 1 + 1:nrow(comparison[[.x]]),
                           cols = stringr::str_which(colnames(comparison[[.x]]), "\\.df2$"),
                           gridExpand = TRUE)
      }

      # conditionally apply addition/deletion formatting for row and value changes
      if (.x %in% c("all", "all_lr")) {

        # whole rows (additions and deletions only)
        if (.x == "all") {

          # addition rows
          add_rows <- which(
            stringr::str_detect(comparison[[.x]]$discrepancy, "addition") &
            comparison[[.x]]$source == "df1"
          )
          openxlsx::addStyle(
            wb,
            sheet = sname,
            style = additionStyle1,
            rows = 1 + add_rows,
            cols = seq(which(colnames(comparison[[.x]]) == freeCols[1]),
                       ncol(comparison[[.x]])),
            gridExpand = TRUE
          )

          # deletion rows, df2
          del_rows <- which(
            stringr::str_detect(comparison[[.x]]$discrepancy, "deletion") &
              comparison[[.x]]$source == "df2"
          )
          openxlsx::addStyle(
            wb,
            sheet = sname,
            style = deletionStyle2,
            rows = 1 + del_rows,
            cols = seq(which(colnames(comparison[[.x]]) == freeCols[1]),
                       ncol(comparison[[.x]])),
            gridExpand = TRUE
          )

        } else if (.x == "all_lr") {

          # addition rows
          add_rows <- stringr::str_which(comparison[[.x]]$discrepancy, "addition")

          # for df1 columns
          add_cols1 <- colnames(comparison[[.x]]) %>%
            stringr::str_which("\\.df1$")
          openxlsx::addStyle(
            wb,
            sheet = sname,
            style = additionStyle1,
            rows = 1 + add_rows,
            cols = add_cols1,
            gridExpand = TRUE
          )

          # for df2 columns
          add_cols2 <- colnames(comparison[[.x]]) %>%
            stringr::str_which("\\.df2$")
          openxlsx::addStyle(
            wb,
            sheet = sname,
            style = additionStyle2,
            rows = 1 + add_rows,
            cols = add_cols2,
            gridExpand = TRUE
          )

          # deletion rows
          del_rows <- stringr::str_which(comparison[[.x]]$discrepancy, "deletion")

          # for df1 columns
          del_cols1 <- colnames(comparison[[.x]]) %>%
            stringr::str_which("\\.df1$")
          openxlsx::addStyle(
            wb,
            sheet = sname,
            style = deletionStyle1,
            rows = 1 + del_rows,
            cols = del_cols1,
            gridExpand = TRUE
          )

          # for df2 columns
          del_cols2 <- colnames(comparison[[.x]]) %>%
            stringr::str_which("\\.df2$")
          openxlsx::addStyle(
            wb,
            sheet = sname,
            style = deletionStyle2,
            rows = 1 + del_rows,
            cols = del_cols2,
            gridExpand = TRUE
          )
        }

        # for all_lr, also color ID duplicate and ID NA rows based on the source
        if (.x == "all_lr") {

          # if from df1, color green
          id_prob_rows1 <- which(comparison[[.x]]$discrepancy %in%
                                  c("df1 ID duplicate (not exact duplicate)",
                                    "df1 ID contains `NA`"))
          openxlsx::addStyle(
            wb,
            sheet = sname,
            style = additionStyle1,
            rows = 1 + id_prob_rows1,
            cols = stringr::str_which(colnames(comparison[[.x]]), "\\.df1$"),
            gridExpand = TRUE
          )
          openxlsx::addStyle(
            wb,
            sheet = sname,
            style = additionStyle2,
            rows = 1 + id_prob_rows1,
            cols = stringr::str_which(colnames(comparison[[.x]]), "\\.df2$"),
            gridExpand = TRUE
          )

          # if from df2, color red
          id_prob_rows2 <- which(comparison[[.x]]$discrepancy %in%
                                  c("df2 ID duplicate (not exact duplicate)",
                                    "df2 ID contains `NA`"))
          openxlsx::addStyle(
            wb,
            sheet = sname,
            style = additionStyle1,
            rows = 1 + id_prob_rows2,
            cols = stringr::str_which(colnames(comparison[[.x]]), "\\.df1$"),
            gridExpand = TRUE
          )
          openxlsx::addStyle(
            wb,
            sheet = sname,
            style = additionStyle2,
            rows = 1 + id_prob_rows2,
            cols = stringr::str_which(colnames(comparison[[.x]]), "\\.df2$"),
            gridExpand = TRUE
          )
        }

        # values in change rows
        if (.x == "all") {
          index_lookup <- comparison$all_index_by_col

        } else if (.x == "all_lr") {
          index_lookup <- comparison$all_index_by_col_lr

        } else {
          stop("x must be one of `c('all', 'changed', 'change_lr')`")
        }

        for (el in seq_along(index_lookup)) {

          if (all(is.na(index_lookup[[el]]))) { next }

          if (.x == "all") {
            cols <- which(colnames(comparison[[.x]]) == names(index_lookup)[el])
            df1_rows <- 1 + which(comparison[[.x]]$source == "df1")
            df2_rows <- 1 + which(comparison[[.x]]$source == "df2")
            change_rows <- 1 + index_lookup[[el]]
            add_rows <- intersect(df1_rows, change_rows)
            del_rows <- intersect(df2_rows, change_rows)
            openxlsx::addStyle(wb,
                               sheet = sname,
                               style = additionStyle1,
                               rows = add_rows,
                               cols = cols,
                               gridExpand = TRUE)
            openxlsx::addStyle(wb,
                               sheet = sname,
                               style = deletionStyle2,
                               rows = del_rows,
                               cols = cols,
                               gridExpand = TRUE)

          } else if (.x == "all_lr") {

            rows <- 1 + index_lookup[[el]]
            add_cols <- paste0(names(index_lookup)[el], ".df1")
            add_cols <- which(colnames(comparison[[.x]]) == add_cols)
            del_cols <- paste0(names(index_lookup)[el], ".df2")
            del_cols <- which(colnames(comparison[[.x]]) == del_cols)
            openxlsx::addStyle(wb,
                               sheet = sname,
                               style = additionStyle1,
                               rows = rows,
                               cols = add_cols,
                               gridExpand = TRUE)
            openxlsx::addStyle(wb,
                               sheet = sname,
                               style = deletionStyle2,
                               rows = rows,
                               cols = del_cols,
                               gridExpand = TRUE)
          }
        } # end of for loop through index lookup
      } # end of if statement for all, changed, and changed_lr comparison elements


      # conditionally color added/deleted whole columns
      if (!.x %in% c("df1", "df2")) {
        if (.x == "all") {

          # cols added in df1
          openxlsx::addStyle(
            wb,
            sheet = sname,
            style = additionStyle1,
            rows = 1 + which(comparison[[.x]]$source == "df1"),
            cols = which(colnames(comparison[[.x]]) %in% comparison$cc_out$df1_only),
            gridExpand = TRUE
          )
          openxlsx::addStyle(
            wb,
            sheet = sname,
            style = additionStyle2,
            rows = 1 + which(comparison[[.x]]$source == "df2"),
            cols = which(colnames(comparison[[.x]]) %in% comparison$cc_out$df1_only),
            gridExpand = TRUE
          )

          # cols in df2 but not in df1
          openxlsx::addStyle(
            wb,
            sheet = sname,
            style = deletionStyle1,
            rows = 1 + which(comparison[[.x]]$source == "df1"),
            cols = which(colnames(comparison[[.x]]) %in% comparison$cc_out$df2_only),
            gridExpand = TRUE
          )
          openxlsx::addStyle(
            wb,
            sheet = sname,
            style = deletionStyle2,
            rows = 1 + which(comparison[[.x]]$source == "df2"),
            cols = which(colnames(comparison[[.x]]) %in% comparison$cc_out$df2_only),
            gridExpand = TRUE
          )

          # for the changed left/right view
        } else if (.x == "all_lr") {

          # cols added in df1
          openxlsx::addStyle(
            wb,
            sheet = sname,
            style = additionStyle1,
            rows = 2:(nrow(comparison[[.x]]) + 1),
            cols = which(colnames(comparison[[.x]]) %in% paste0(comparison$cc_out$df1_only, ".df1")),
            gridExpand = TRUE
          )
          openxlsx::addStyle(
            wb,
            sheet = sname,
            style = deletionStyle2,
            rows = 2:(nrow(comparison[[.x]]) + 1),
            cols = which(colnames(comparison[[.x]]) %in% paste0(comparison$cc_out$df1_only, ".df2")),
            gridExpand = TRUE
          )

          # cols in df2 but not df1
          openxlsx::addStyle(
            wb,
            sheet = sname,
            style = deletionStyle2,
            rows = 2:(nrow(comparison[[.x]]) + 1),
            cols = which(colnames(comparison[[.x]]) %in% paste0(comparison$cc_out$df2_only, ".df2")),
            gridExpand = TRUE
          )
          openxlsx::addStyle(
            wb,
            sheet = sname,
            style = additionStyle1,
            rows = 2:(nrow(comparison[[.x]]) + 1),
            cols = which(colnames(comparison[[.x]]) %in% paste0(comparison$cc_out$df2_only, ".df1")),
            gridExpand = TRUE
          )
        }
      } # end of if statement to format added/deleted whole columns

      # group columns
      if (.x == "all") {
        grouped <- c("change group", "exact dup cnt", "ID dup cnt")
        openxlsx::groupColumns(wb,
                               sheet = sname,
                               cols = which(colnames(comparison[[.x]]) %in% grouped),
                               hidden = TRUE)
      } else if (.x == "all_lr") {
        grouped <- c("change group",
                     "exact dup cnt.df1", "exact dup cnt.df2",
                     "ID dup cnt.df1", "ID dup cnt.df2")
        openxlsx::groupColumns(wb,
                               sheet = sname,
                               cols = which(colnames(comparison[[.x]]) %in% grouped),
                               hidden = TRUE)
      }
    })

  # save the file
  openxlsx::saveWorkbook(wb,
                         file = path,
                         overwrite = T)

  # open the file if requested
  if (autoOpen) {
    system("cmd.exe", input = paste0("start excel \"", path, "\""))
  }

}