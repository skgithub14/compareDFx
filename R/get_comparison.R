#' Compare two data frames
#'
#' Generates a detailed comparison report of two data frames
#'
#' @param df1,df2 data frames to compare
#' @param id_cols the column names in `df1` and `df2` that make up a unique ID
#' @param tolerance the amount by which two numbers can differ to be considered
#'  equal, default is `0.00001`
#'
#' @returns a named list with elements:
#'  * `col_summary_simple`: summary statistics for columns
#'  * `col_summary_by_col`: summary statistics by column
#'  * `row_summary`: summary statistics for rows
#'  * `all_tb`: comparison data displayed where each row in `df1` is shown
#'   directly above its `df2` counterpart (a top-bottom view) with comparison
#'   annotation columns
#'  * `all_lr`: comparison data displayed where each column in `df1` is shown
#'   to the left of its `df2` counterpart (a left-right view) with comparison
#'   annotation columns
#'  * `all_tb_change_indices`: a named list where the names are the data columns
#'   in `all_tb` and the elements are numeric vectors of the row indices that
#'   changed between `df1` and `df2` in a column
#'  * `all_lr_change_indices`: a named list where the names are the data columns
#'   in `all_lr` and the elements are numeric vectors of the row indices that
#'   changed between `df1` and `df2` in a column
#'  * `id_cols`: the columns in `df1` and `df2` that form a unique row ID
#'  * `cc_out`: a name list with four elements:
#'    * `same`: a logical indicating whether the column names in `df1` and `df2`
#'     are the same
#'    * `both`: a character vector of column names that are in both `df1` and
#'     `df2`
#'    * `df1_only`: character vector of column names that are in `df1` but not
#'     `df2`
#'    * `df2_only`: character vector of column names that are in `df2` but not
#'     `df1`
#'  * `df1`: the raw data from `df1`
#'  * `df2`: the raw data from `df2`
#'
#' @export
#'
get_comparison <- function(df1, df2, id_cols, tolerance = 0.00001) {

  # compare the column names
  cc_out <- compare_cols(df1 = df1, df2 = df2)

  # if there were different columns, add dummy columns as needed
  df_list <- insert_dummy_cols(df1 = df1,
                               df2 = df2,
                               cc_out = cc_out,
                               id_cols = id_cols)

  # standardize the order of the columns
  standard_cols <- standardize_col_order(df1 = df_list$df1,
                                         df2 = df_list$df2,
                                         id_cols = id_cols)

  # separate out exact duplicates, ID only duplicates, and IDs with NA
  dup_list1 <- find_dups(df = standard_cols$df1,
                         id_cols = id_cols,
                         source = "df1")
  dup_list2 <- find_dups(df = standard_cols$df2,
                         id_cols = id_cols,
                         source = "df2")

  # compare data frames on de-duplicated data
  comp <- compare_df_wrapper(df1_no_dups = dup_list1$no_dups,
                             df2_no_dups = dup_list2$no_dups,
                             id_cols = id_cols,
                             tolerance = tolerance)

  # create the top/bottom view of all data
  all_tb <- get_top_bottom_view(comparison_df = comp$comparison_df,
                                comparison_table_diff = comp$comparison_table_diff,
                                id_cols = id_cols,
                                cc_out = cc_out,
                                df1_exact_dups = dup_list1$exact_dups,
                                df2_exact_dups = dup_list2$exact_dups,
                                df1_id_dups = dup_list1$id_dups,
                                df2_id_dups = dup_list2$id_dups,
                                df1_id_NA = dup_list1$id_NA,
                                df2_id_NA = dup_list2$id_NA)

  # create left/right view of all data
  all_lr <- get_left_right_view(top_bottom_view = all_tb)

  # find row indices of all changes by column
  changed_indices <-
    get_changed_row_indices_by_column(top_bottom_view = all_tb,
                                      left_right_view = all_lr,
                                      comparison_table_diff = comp$comparison_table_diff,
                                      id_cols = id_cols)

  # column comparison report
  col_summary <- summarize_compare_cols(df1 = df1,
                                        df2 = df2,
                                        id_cols = id_cols,
                                        cc_out = cc_out,
                                        df_standard_cols = standard_cols,
                                        comparison_table_diff = comp$comparison_table_diff)

  # row comparison report
  row_summary <- summarize_compare_rows(df1 = df1,
                                        df2 = df2,
                                        all = all_tb,
                                        id_cols = id_cols)

  # issue warnings and messages
  if (any(stringr::str_detect(all_tb$discrepancy, "ID duplicate"))) {
    warning("ID duplicates detected, recommend fixing these and re-running `get_comparison()`")
  }
  if (any(stringr::str_detect(all_tb$discrepancy, "ID contains `NA`"))) {
    warning("ID columns contain `NA`, recommend fixing these and re-running `get_comparison()`")
  }
  if (!cc_out$same) {
    message("df1 and df2 have different columns therefore no records are recorded as 'matched'")
  }
  if (all(all_tb$discrepancy == "matched")) {
    message("All records in df1 and df2 matched")
  }

  return(
    list(
      col_summary_simple = col_summary$simple,
      col_summary_by_col = col_summary$by_col,
      row_summary = row_summary,
      all_tb = all_tb,
      all_lr = all_lr,
      all_tb_change_indices = changed_indices$top_bottom,
      all_lr_change_indices = changed_indices$left_right,
      id_cols = id_cols,
      cc_out = cc_out,
      df1 = df1,
      df2 = df2
    )
  )
}


#' Compare column names of two data frames to find differences
#'
#' Indicates if two data frames have the same column names or not and what the
#' differences are.
#'
#' @param df1,df2 data frames to compare
#'
#' @returns a named list with 4 elements:
#'  * `same`: a logical indicating whether `df1` and `df2` have the same columns
#'  * `both`: a character vector with the column names in both `df1` and `df2`
#'  * `df1_only`: a character vector with the column names only in `df1`
#'  * `df2_only`: a character vector with the column names only in `df2`
#'
compare_cols <- function(df1, df2) {
  both <- intersect(colnames(df1), colnames(df2))
  df1_only <- setdiff(colnames(df1), colnames(df2))
  df2_only <- setdiff(colnames(df2), colnames(df1))

  if (length(df1_only) == 0 & length(df2_only) == 0) {
    same <- TRUE
  } else {
    same <- FALSE
  }

  return(
    list(
      same = same,
      both = both,
      df1_only = df1_only,
      df2_only = df2_only
    )
  )
}


#' Force two data frames to have the same columns by creating dummy columns
#'
#' For each column in data frame 1 that is missing in data frame 2, this
#' function creates the missing column in data frame 2 and fills it with `NA`,
#' and vice-version. It also makes the classes dummy columns match and ensure
#' both data frames have the same column order.
#'
#' @inheritParams compare_cols
#' @param cc_out a list output from [compare_cols()]
#' @param id_cols a character vector of id columns output from [compare_cols()]
#'
#' @returns a named list with two elements:
#'  * `df1`: the modified `df1` data frame
#'  * `df2`: the modified `df2` data frame
#'
insert_dummy_cols <- function(df1, df2, cc_out, id_cols) {
  # do nothing if they already have the same columns
  if (cc_out$same) {
    return(
      list(
        df1 = df1,
        df2 = df2
      )
    )
  }

  # create missing columns with correct classes via utility function
  make_cols <- function(ref_df, work_df, make_cols) {
    if (length(make_cols) > 0) {
      for (col in make_cols) {
        ref_class <- class(ref_df[[col]])
        if (ref_class == "numeric") {
          work_df[[col]] <- NA_real_
        } else if (ref_class == "integer") {
          work_df[[col]] <- NA_integer_
        } else {
          work_df[[col]] <- NA_character_
        }
      }
    }
    return(work_df)
  }

  df1 <- make_cols(ref_df = df2, work_df = df1, make_cols = cc_out$df2_only)
  df2 <- make_cols(ref_df = df1, work_df = df2, make_cols = cc_out$df1_only)

  return(
    list(
      df1 = df1,
      df2 = df2
    )
  )
}


#' Standardize column order
#'
#' Puts the columns of two data frames in the same order and places the unique
#' identifiers columns to the far left of the data frame. Data frames must have
#' the exact same columns (if not, apply [insert_dummy_cols()] first)
#'
#' @inheritParams compare_cols
#' @inheritParams insert_dummy_cols
#'
#' @returns a list with two elements:
#'  * `df1`: a modified data frame `df1`
#'  * `df2`: a modified data frame `df2`
#'
standardize_col_order <- function(df1, df2, id_cols) {
  not_id_cols <- setdiff(colnames(df1), id_cols)

  df1 <- dplyr::select(
    df1,
    tidyselect::all_of(id_cols),
    tidyselect::all_of(not_id_cols)
  )

  df2 <- dplyr::select(df2, tidyselect::all_of(colnames(df1)))

  return(
    list(
      df1 = df1,
      df2 = df2
    )
  )
}

#' Separate duplicated data frame rows from rows with no duplicates
#'
#' Separates a data frame into sub-data frames based on types of row duplication
#' or lack thereof. Also identifies `NA` values in `id_cols`.
#'
#' @param df a data frame with or without duplicated rows
#' @param id_cols a character vector of the column names that form a unique ID
#' @param source a string, name of the data source (usually `"df1"` or `"df2"`)
#'
#' @returns a names list with four elements:
#'  * `exact_dups`: rows which were duplicated for every value, there will be
#'   one distinct row in `exact_dups` for each case and the column `n` will
#'   list how many occurences of the row were in `df`.
#'  * `id_dups`: rows which were duplicated based on `id_cols` but had other
#'   values that differed, the column `n` contains how many ID duplicates of
#'   one ID exist in `df`
#'  * `id_NA`: rows which had an `NA` value in a `id_cols` column
#'  * `no_dups`: this is `df` with all `id_dups` row removed and only one copy
#'   of each `exact_dups` row.
#'
find_dups <- function(df, id_cols, source) {

  # check for id columns with NA values
  id_NA <- df %>%
    dplyr::filter(dplyr::if_any(
      .cols = tidyselect::all_of(id_cols),
      ~ is.na(.)
    )) %>%
    dplyr::mutate(`ID dup cnt` = NA_real_,
                  .after = id_cols[length(id_cols)]) %>%
    dplyr::mutate(`exact dup cnt` = NA_real_,
                  .after = id_cols[length(id_cols)]) %>%
    dplyr::mutate(discrepancy = "ID contains `NA`",
                  .after = id_cols[length(id_cols)]) %>%
    dplyr::mutate(source = source, .before = id_cols[1])

  # duplicates that match on every column
  exact_dups <- df %>%
    dplyr::anti_join(dplyr::select(id_NA, -discrepancy),
                     by = intersect(colnames(df),
                                    setdiff(colnames(id_NA), "discrepancy"))
    ) %>%
    dplyr::group_by(dplyr::across(tidyselect::everything())) %>%
    dplyr::mutate(`exact dup cnt` = dplyr::row_number(),
                  .after = id_cols[length(id_cols)]) %>%
    dplyr::ungroup() %>%
    dplyr::filter(`exact dup cnt` > 1) %>%
    dplyr::slice_max(`exact dup cnt`) %>%
    dplyr::distinct() %>%
    dplyr::mutate(`ID dup cnt` = NA_real_,
                  .after = `exact dup cnt`) %>%
    dplyr::mutate(discrepancy = "exact duplicate",
                  .after = id_cols[length(id_cols)]) %>%
    dplyr::mutate(source = source, .before = id_cols[1])

  # duplicates that match only by the ID columns but have other differences
  id_dups <- df %>%
    dplyr::anti_join(dplyr::select(id_NA, -discrepancy),
                     by = intersect(colnames(df),
                                    setdiff(colnames(id_NA), "discrepancy"))
    ) %>%
    dplyr::anti_join(dplyr::select(exact_dups, -c(discrepancy, `exact dup cnt`)),
                     by = intersect(colnames(df),
                                    setdiff(colnames(df), c("discrepancy",
                                                            "exact dup cnt")))
    ) %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(id_cols))) %>%
    dplyr::mutate(`ID dup cnt` = dplyr::n(),
                  .after = id_cols[length(id_cols)]) %>%
    dplyr::filter(`ID dup cnt` > 1) %>%
    dplyr::ungroup()  %>%
    dplyr::mutate(`exact dup cnt` = NA_real_,
                  .after = id_cols[length(id_cols)]) %>%
    dplyr::mutate(discrepancy = "ID duplicate (not exact duplicate)",
                  .after = id_cols[length(id_cols)]) %>%
    dplyr::mutate(source = source, .before = id_cols[1])

  # data frame with duplicates removed:
  #  * ID duplicates are removed because they will have multiple matches
  #  * ID NAs are removed because they may not match correctly
  #  * Exact match duplicates are reduced to one instance
  #  * ids with NA values are retained
  no_dups <- df %>%
    dplyr::anti_join(dplyr::select(id_NA, -discrepancy),
                     by = intersect(colnames(df),
                                    setdiff(colnames(id_NA), "discrepancy"))
    ) %>%
    dplyr::anti_join(dplyr::select(id_dups, -c(discrepancy, `ID dup cnt`)),
                     by = id_cols) %>%
    dplyr::distinct() %>%
    dplyr::mutate(`ID dup cnt` = NA_real_,
                  .after = id_cols[length(id_cols)]) %>%
    dplyr::mutate(discrepancy = NA_character_,
                  .after = id_cols[length(id_cols)]) %>%
    dplyr::mutate(source = source, .before = id_cols[1])

  return(
    list(
      exact_dups = exact_dups,
      id_dups = id_dups,
      id_NA = id_NA,
      no_dups = no_dups
    )
  )
}


#' Compare two data frames using [compareDF::compare_df()]
#'
#' A convenience wrapper for [compareDF::compare_df()]
#'
#' Rows that were duplicated in `df1` or `df2` should be removed prior to calling
#' this function
#'
#' @param df1_no_dups,df2_no_dups a `no_dups` list element from a [find_dups()]
#' output, for `df1` and `df2`, respectively
#' @inheritParams insert_dummy_cols
#' @param tolerance amount by which numeric values can vary and still be
#'  considered equal (helps reduce machine rounding errors false positive
#'  differences)
#'
#' @returns a list returned by [compareDF::compare_df()]
#'
compare_df_wrapper <- function(df1_no_dups,
                               df2_no_dups,
                               id_cols,
                               tolerance) {

  # make round_output_to match the tolerance
  if (tolerance > 1) {
    rnd <- 1
  } else {
    tol_chr <- as.character(tolerance)
    tol_chr <- stringr::str_replace(tol_chr, "^\\.", "0.")
    rnd <- nchar(tol_chr) - 2
  }

  suppressWarnings(
    suppressMessages(
      comp <- compareDF::compare_df(
        df1_no_dups,
        df2_no_dups,
        group_col = id_cols,
        exclude = c("source", "discrepancy", "ID dup cnt", "exact dup cnt"),
        tolerance_type = "difference",
        tolerance = tolerance,
        stop_on_error = FALSE,
        keep_unchanged_rows = TRUE,
        round_output_to = rnd
      )
    )
  )

  # transfer group numbers to comparison_table_diff for future cals
  comp$comparison_table_diff$grp <- comp$comparison_df$grp

  return(comp)
}


#' Segregates data frame comparisons by type
#'
#' Splits the output of [compare_df_wrapper()] in several smaller data frames
#' organized by type of change and formats it
#'
#' @inheritParams compare_cols
#' @inheritParams insert_dummy_cols
#' @param comparison_df the `comparison_df` element of a [compare_df_wrapper()]
#' output
#' @param comparison_table_diff the `comparison_table_diff` element of a
#' [compare_df_wrapper()] output
#' @param df1_exact_dups,df2_exact_dups an `exact_dups` list element from a
#' [find_dups()] output, for `df1` and `df2`, respectively
#' @param df1_id_dups,df2_id_dups an `id_dups` list element from a [find_dups()]
#' output, for `df1` and `df2`, respectively
#' @param df1_id_NA,df2_id_NA an `id_NA` list element from a [find_dups()]
#' output, for `df1` and `df2`, respectively
#'
#' @returns a data frame with all data from `df1` and `df2` with comparison
#'  annotations where each row in `df1` is displayed directly above its `df2`
#'  counterpart
#'
get_top_bottom_view <- function(comparison_df,
                                comparison_table_diff,
                                id_cols,
                                cc_out,
                                df1_exact_dups,
                                df2_exact_dups,
                                df1_id_dups,
                                df2_id_dups,
                                df1_id_NA,
                                df2_id_NA) {

  all_dat1 <- comparison_df %>%
    dplyr::group_by(grp) %>%
    dplyr::mutate(n = dplyr::row_number(), .after = "grp") %>%

    # mark which row came from what data frame
    dplyr::mutate(
      source = dplyr::case_when(
        all(chng_type == "+") ~ "df1",
        all(chng_type == "-") ~ "df2",
        chng_type == "=" & n == 1 ~ "df1",
        chng_type == "=" & n == 2 ~ "df2",
        chng_type == "+" & max(n, na.rm = T) == 2 ~ "df1",
        chng_type == "-" & max(n, na.rm = T) == 2 ~ "df2",
        TRUE ~ NA_character_
      ),
      .before = id_cols[1]
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-n) %>%

    # check for unhandled cases
    {
      if (any(is.na(.$source))) {
        stop("There are unhandled cases in the source column marked as `NA`")
      } else {
        .
      }
    }

  # merge in exact duplicate counts
  dup_cnt1 <- dplyr::select(df1_exact_dups,
                            tidyselect::all_of(id_cols),
                            `exact dup cnt`,
                            source)
  dup_cnt2 <- dplyr::select(df2_exact_dups,
                            tidyselect::all_of(id_cols),
                            `exact dup cnt`,
                            source)
  dup_cnt <- dplyr::bind_rows(dup_cnt1, dup_cnt2)

  all_dat2 <- all_dat1 %>%
    dplyr::left_join(dup_cnt, by = c(id_cols, "source")) %>%
    dplyr::relocate(`exact dup cnt`, .after = source) %>%

    # mark whether a pure addition, deletion, or change
    dplyr::group_by(grp) %>%
    dplyr::mutate(
      discrepancy = dplyr::case_when(
        all(chng_type == "=") ~ "matched",
        all(chng_type == "+") ~ "addition",
        all(chng_type == "-") ~ "deletion",
        all(chng_type %in% c("-", "+")) ~ "changed",
        TRUE ~ NA_character_
      ),
      .before = source
    ) %>%
    dplyr::select(-chng_type) %>%

    # check for unhandled cases
    {
      if (any(is.na(.$discrepancy))) {
        stop("There are unhandled cases in the discrepancy column marked as `NA`")
      } else {
        .
      }
    } %>%
    dplyr::ungroup() %>%

    # handle cases when there is duplicate and a match/change/add/del
    dplyr::mutate(
      discrepancy = dplyr::if_else(
        !is.na(`exact dup cnt`),
        paste0(discrepancy, ", ", "exact duplicate"),
        discrepancy
      )
    ) %>%

    # change grp column to "change group" column, only populated for changes
    dplyr::relocate(grp, .after = source) %>%
    dplyr::mutate(
      grp = dplyr::if_else(stringr::str_detect(discrepancy, "changed"),
                           grp,
                           NA_real_)
    ) %>%
    dplyr::rename(`change group` = grp) %>%

    # create dummy ID dup column
    dplyr::mutate(`ID dup cnt` = NA_real_,
                  .after = `exact dup cnt`) %>%

    # arrange by ID columns
    dplyr::arrange(dplyr::across(tidyselect::all_of(id_cols)))

  ## for all data, add in the ID NA and ID dup segments
  # force the classes to match
  for (df_num in 1:2) {
    df_id_dups <- get(paste0("df", df_num, "_id_dups"))
    df_id_NA <- get(paste0("df", df_num, "_id_NA"))
    df_exact_dups <- get(paste0("df", df_num, "_exact_dups"))

    for (coln in colnames(df_id_dups)) {
      all_dat_class <- class(all_dat2[[coln]])
      df_class <- class(df_id_dups[[coln]])

      if (all_dat_class != df_class) {
        if (all_dat_class %in% c("integer", "numeric", "logical")) {
          as <- paste0("as.", all_dat_class)
        } else {
          as <- "as.character"
        }

        df_id_dups[[coln]] <- do.call(as, list(x = df_id_dups[[coln]]))
        df_id_NA[[coln]] <- do.call(as, list(x = df_id_NA[[coln]]))
        df_exact_dups[[coln]] <- do.call(as, list(x = df_exact_dups[[coln]]))

        assign(paste0("df", df_num, "_id_dups"), df_id_dups)
        assign(paste0("df", df_num, "_id_NA"), df_id_NA)
        assign(paste0("df", df_num, "_exact_dups"), df_exact_dups)
      }
    }
  }

  all_dat3 <- all_dat2 %>%
    dplyr::bind_rows(df1_id_dups) %>%
    dplyr::bind_rows(df2_id_dups) %>%
    dplyr::bind_rows(df1_id_NA) %>%
    dplyr::bind_rows(df2_id_NA) %>%
    dplyr::arrange(dplyr::across(c(tidyselect::all_of(id_cols), source)))

  return(all_dat3)
}


#' Convert data comparison from top-bottom to left-right view
#'
#' Takes a comparison data frame produced in [get_comparison()] and manipulates
#' it so that it displays each `df1` columns to the left of its `df2`
#' counterpart.
#'
#' @inheritParams get_changed_row_indices_by_column
#'
#' @returns a modified copy of `top_bottom_view` that displays `df1` data to
#'  the left of the `df2` data instead of above the `df2` data
#'
get_left_right_view <- function(top_bottom_view) {

  # first create dummy rows for additions and deletions
  all_lr_tmp <- top_bottom_view %>%
    dplyr::mutate(
      discrepancy = dplyr::case_when(
        stringr::str_detect(discrepancy, "ID duplicate") ~
          stringr::str_replace(discrepancy, "ID duplicate", paste(source, "ID duplicate")),
        stringr::str_detect(discrepancy, "ID contains `NA`") ~
          stringr::str_replace(discrepancy, "ID contains `NA`", paste(source, "ID contains `NA`")),
        TRUE ~ discrepancy,
      ),
      tmp_col = dplyr::if_else(
        stringr::str_detect(discrepancy,
                            "addition|deletion|ID duplicate|ID contains `NA`"),
        "orig,dummy",
        "orig"
      )
    ) %>%
    tidyr::separate_rows(tmp_col, sep = ",") %>%
    dplyr::mutate(
      source = dplyr::case_when(
        tmp_col == "dummy" & source == "df1" ~ "df2",
        tmp_col == "dummy" & source == "df2" ~ "df1",
        TRUE ~ source
      ),
      dplyr::across(c(`exact dup cnt`, `ID dup cnt`),
                    ~ dplyr::if_else(
                      tmp_col == "dummy",
                      NA_real_,
                      .
                    )),
      dplyr::across(-c(source,
                       discrepancy,
                       `change group`,
                       `exact dup cnt`,
                       `ID dup cnt`,
                       tidyselect::all_of(id_cols)),
                    ~ dplyr::if_else(tmp_col == "dummy", NA, .)
      )
    ) %>%
    dplyr::select(-tmp_col) %>%
    dplyr::arrange(dplyr::across(c(tidyselect::all_of(id_cols), source)))

  all_lr_df2 <- all_lr_tmp %>%
    dplyr::filter(source == "df2") %>%
    dplyr::select(-c(
      source,
      discrepancy,
      `change group`,
      tidyselect::all_of(id_cols)
    )) %>%
    dplyr::rename_with(
      .cols = tidyselect::everything(),
      ~ paste0(., ".df2")
    )

  all_lr_df1 <- all_lr_tmp %>%
    dplyr::filter(source == "df1") %>%
    dplyr::rename_with(
      .cols = -c(
        source,
        discrepancy,
        `change group`,
        tidyselect::all_of(id_cols)
      ),
      ~ paste0(., ".df1")
    ) %>%
    dplyr::select(-source)

  all_lr_col_order <- colnames(all_lr_df2) %>%
    purrr::map(~ rep(.x, 2)) %>%
    unlist() %>%
    purrr::imap(~ {
      if (.y %% 2 == 1) {
        stringr::str_replace(.x, "\\.df2$", ".df1")
      } else {
        .x
      }
    }) %>%
    unlist()

  all_lr <- all_lr_df1 %>%
    dplyr::bind_cols(all_lr_df2) %>%
    dplyr::select(
      discrepancy,
      `change group`,
      tidyselect::all_of(id_cols),
      tidyselect::all_of(all_lr_col_order)
    ) %>%
    dplyr::relocate(tidyselect::all_of(id_cols), .after = `ID dup cnt.df2`)

  return(all_lr)
}


#' Get row indices by column of changed values
#'
#' Given both top-bottom and left-right comparison data frames, this function
#' determines the row indices for each comparison data frame, by column, that
#' have changed between `df1` and `df2`
#'
#' @param top_bottom_view a data frame of comparison data where each row in
#'  `df1` is displayed directly above its `df2` counterpart
#' @param left_right_view a data frame of comparison data where each column in
#'  `df1` is displayed directly to the left of its `df2` counterpart
#' @param comparison_table_diff a data frame returned by the
#'  `comparison_table_diff` element of a [compare_df_wrapper()] output
#' @inheritParams insert_dummy_cols
#'
#' @returns a named list where each element's name is a column in
#'  `comparison_data` and the elements themselves are numeric vectors with the
#'  row indices that changed
#'
get_changed_row_indices_by_column <- function(top_bottom_view,
                                              left_right_view,
                                              comparison_table_diff,
                                              id_cols) {

  change_ids <- top_bottom_view %>%
    dplyr::filter(stringr::str_detect(discrepancy, "changed")) %>%
    dplyr::select(`change group`, tidyselect::all_of(id_cols)) %>%
    dplyr::distinct()
  change_locations <- comparison_table_diff %>%
    dplyr::select(-tidyselect::all_of(id_cols)) %>%
    dplyr::left_join(change_ids, by = c("grp" = "change group")) %>%
    dplyr::relocate(tidyselect::all_of(id_cols), .after = "grp") %>%
    dplyr::filter(dplyr::if_all(tidyselect::all_of(id_cols), ~ !is.na(.))) %>%
    dplyr::group_by(grp) %>%
    dplyr::mutate(n = dplyr::row_number(), .after = "grp") %>%
    dplyr::mutate(
      source = dplyr::case_when(
        all(chng_type == "+") ~ "df1",
        all(chng_type == "-") ~ "df2",
        chng_type == "=" & n == 1 ~ "df1",
        chng_type == "=" & n == 2 ~ "df2",
        chng_type == "+" & max(n, na.rm = T) == 2 ~ "df1",
        chng_type == "-" & max(n, na.rm = T) == 2 ~ "df2",
        TRUE ~ NA_character_
      ),
      .before = id_cols[1]
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(source == "df1") %>%
    dplyr::select(-c(n, source, chng_type)) %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.character),
                                ~ dplyr::na_if(., "="))) %>%
    dplyr::mutate(dplyr::across(.cols = -c(grp, tidyselect::all_of(id_cols)),
                                ~ dplyr::if_else(
                                  !is.na(.),
                                  as.character(grp),
                                  .
                                ))
    )

  # get change indices for the all and all_lr in separate lists which can
  #  be used by `create_comparison_excel()` to color code changes
  non_index_cols <- setdiff(colnames(change_locations), c("grp", id_cols))
  get_indices_by_data_frame <- function (df, non_index_cols, change_locations) {
    tmp_df <- dplyr::mutate(df,
                            index = dplyr::row_number())
    index_by_col <- vector(mode = "list", length = length(non_index_cols)) %>%
      setNames(non_index_cols)
    for (col in non_index_cols) {
      grps <- change_locations[[col]]
      grps <- grps[which(!is.na(grps))]
      grps <- as.numeric(grps)
      if (length(grps) > 0) {
        ind <- tmp_df$index[which(tmp_df$`change group` %in% grps)]
      } else {
        ind <- NA_real_
      }
      index_by_col[[col]] <- ind
    }
    return(index_by_col)
  }

  top_bottom_indices <-
    get_indices_by_data_frame(df = top_bottom_view,
                              non_index_cols = non_index_cols,
                              change_locations = change_locations)
  left_right_indices <-
    get_indices_by_data_frame(df = left_right_view,
                              non_index_cols = non_index_cols,
                              change_locations = change_locations)

  return(
    list(
      top_bottom = top_bottom_indices,
      left_right = left_right_indices
    )
  )
}


#' Summary metrics for data frame comparison by rows
#'
#' Produces a table of high level summary statistics for comparing two data
#' frames in terms of rows
#'
#' @inheritParams compare_cols
#' @inheritParams insert_dummy_cols
#' @param all a data frame with all the compared data with `discrepancy` column
#'
#' @returns a data frame with three columns: `Rows`, `df1`, `df2`
#'
summarize_compare_rows <- function(df1,
                                   df2,
                                   all,
                                   id_cols) {

  added_cnt <- all %>%
    dplyr::filter(stringr::str_detect(discrepancy, "addition")) %>%
    nrow()

  deleted_cnt <- all %>%
    dplyr::filter(stringr::str_detect(discrepancy, "deletion")) %>%
    nrow()

  changed_cnt <- all %>%
    dplyr::filter(stringr::str_detect(discrepancy, "changed")) %>%
    dplyr::filter(source == "df1") %>%
    nrow()

  matched_cnt <- all %>%
    dplyr::filter(stringr::str_detect(discrepancy, "matched")) %>%
    dplyr::filter(source == "df1") %>%
    nrow()

  exact_dup_cnt1 <- all %>%
    dplyr::filter(stringr::str_detect(discrepancy, "exact duplicate")) %>%
    dplyr::filter(source == "df1") %>%
    dplyr::pull(`exact dup cnt`) %>%
    sum(na.rm = T)

  exact_dup_cnt2 <- all %>%
    dplyr::filter(stringr::str_detect(discrepancy, "exact duplicate")) %>%
    dplyr::filter(source == "df2") %>%
    dplyr::pull(`exact dup cnt`) %>%
    sum(na.rm = T)

  ID_dup_cnt1 <- all %>%
    dplyr::filter(stringr::str_detect(discrepancy, "ID duplicate")) %>%
    dplyr::filter(source == "df1") %>%
    dplyr::distinct(dplyr::across(tidyselect::all_of(id_cols)),
                    .keep_all = T
    ) %>%
    dplyr::pull(`ID dup cnt`) %>%
    sum(na.rm = T)

  ID_dup_cnt2 <- all %>%
    dplyr::filter(stringr::str_detect(discrepancy, "ID duplicate")) %>%
    dplyr::filter(source == "df2") %>%
    dplyr::distinct(dplyr::across(tidyselect::all_of(id_cols)),
                    .keep_all = T
    ) %>%
    dplyr::pull(`ID dup cnt`) %>%
    sum(na.rm = T)

  NA_id_cnt1 <- all %>%
    dplyr::filter(stringr::str_detect(discrepancy, "ID contains `NA`")) %>%
    dplyr::filter(source == "df1") %>%
    nrow()

  NA_id_cnt2 <- all %>%
    dplyr::filter(stringr::str_detect(discrepancy, "ID contains `NA`")) %>%
    dplyr::filter(source == "df2") %>%
    nrow()

  summ_df <- tibble::tribble(
    ~`Rows`, ~`df1`, ~`df2`,
    "Total", nrow(df1), nrow(df2),
    "Added", added_cnt, NA_real_,
    "Deleted", NA_real_, deleted_cnt,
    "Changed", changed_cnt, changed_cnt,
    "Matched", matched_cnt, matched_cnt,
    "Exact duplicates", exact_dup_cnt1, exact_dup_cnt2,
    "ID only duplicates", ID_dup_cnt1, ID_dup_cnt2,
    "`NA` ID values", NA_id_cnt1, NA_id_cnt2
  ) %>%
    dplyr::mutate(
      Issues = dplyr::case_when(
        Rows == "ID only duplicates" & (`df1` > 0 | `df2`) > 0 ~
          "Fix these records and re-run comparison",
        Rows == "`NA` ID values" & (`df1` > 0 | `df2`) > 0 ~
          "Fix these records and re-run comparison",
        TRUE ~ NA_character_
      )
    )

  return(summ_df)
}


#' Summary metrics for data frame comparison by columns
#'
#' Creates two data frames that summarize the differences by columns for two
#' versions of the same data frame. The first provides higher level summary
#' statistics on the number of columns in each data frame, the number of columns
#' with changes, and the number of columns that have the same name but
#' different classes. The second data frame lists by column the number of
#' changes, whether the column was included in the new and/or old data frame,
#' and the original column class.
#'
#' @inheritParams compare_cols
#' @inheritParams insert_dummy_cols
#' @param df_standard_cols a list returned from [standardize_col_order()]
#' @param comparison_table_diff the `comparison_table_diff` list element of a
#' [compare_df_wrapper()] output.
#'
#' @returns a named list containing two data frames:
#'  * `report_simple` a data frame with higher level summary statistics
#'  * `report_by_col` a data frame that lists summary statistics by column name
#'
summarize_compare_cols <- function (df1,
                                    df2,
                                    id_cols,
                                    cc_out,
                                    df_standard_cols,
                                    comparison_table_diff) {

  # counts of changes by column
  col_chng_cnts <- comparison_table_diff %>%

    # remove the additions/deletions
    dplyr::group_by(grp) %>%
    dplyr::filter(!(all(chng_type == "+")) &
                  !(all(chng_type == "-"))) %>%
    dplyr::ungroup() %>%

    # only count each change once
    dplyr::filter(chng_type == "+") %>%

    dplyr::select(-c(grp, chng_type)) %>%
    apply(2, function(x) sum(x == "+"))

  # binary indicator of if a column had changes
  col_chng_ind <- col_chng_cnts %>%
    purrr::map(~ dplyr::if_else(.x > 1, 1, .x)) %>%
    unlist() %>%
    sum(na.rm = T)

  # column classes df1
  col_class1 <- colnames(df_standard_cols$df1) %>%
    stats::setNames(colnames(df_standard_cols$df1)) %>%
    purrr::imap(~ {
      if (.y %in% cc_out$df2_only) {
        NA_character_
      } else {
        class(df1[[.x]])
      }
    }) %>%
    unlist()

  # column classes df2
  col_class2 <- colnames(df_standard_cols$df2) %>%
    stats::setNames(colnames(df_standard_cols$df2)) %>%
    purrr::imap(~ {
      if (.y %in% cc_out$df1_only) {
        NA_character_
      } else {
        class(df2[[.x]])
      }
    }) %>%
    unlist()

  # column class mismatches TRUE/FALSE
  class_match <- col_class1 == col_class2
  col_class_diff_cnt <- sum(!(class_match), na.rm = T)

  # check if a column name was in the original data
  in_df1 <- colnames(df_standard_cols$df1) %in% c(cc_out$both, cc_out$df1_only)
  in_df2 <- colnames(df_standard_cols$df2) %in% c(cc_out$both, cc_out$df2_only)

  # report comparing new to old, high level
  report_simple <- tibble::tribble(
    ~ Columns          , ~ "df1"                , ~ "df2"                ,
    "Total"            , ncol(df1)              , ncol(df2)              ,
    "Added"            , length(cc_out$df1_only), NA_real_               ,
    "Deleted"          , NA_real_               , length(cc_out$df2_only),
    "Changed"          , col_chng_ind           , col_chng_ind           ,
    "Mismatch class"   , col_class_diff_cnt     , col_class_diff_cnt
  ) %>%

    dplyr::mutate(
      Issues = dplyr::case_when(
        Columns == "Added" & `df1` > 0 ~
          "df1 and df2 have different columns therefore no records are 'matched'",
        Columns == "Deleted" & `df2` > 0 ~
          "df1 and df2 have different columns therefore no records are 'matched'",
        TRUE ~ NA_character_
      )
    )

  # a report by column name
  report_by_col <- tibble::tibble(
    Column = colnames(df_standard_cols$df1),
    `In df1` = in_df1,
    `In df2` = in_df2,
    `Changes` = col_chng_cnts,
    `Classes match` = class_match,
    `df1 class` = col_class1,
    `df2 class` = col_class2
  )

  return(
    list(
      simple = report_simple,
      by_col = report_by_col
    )
  )
}


