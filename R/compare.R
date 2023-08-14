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
#' @param cc_out a list output from [compare_col()]
#' @param id_cols a character vector of id columns output from [get_uid_cols()]
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
    dplyr::anti_join(dplyr::select(id_NA, -discrepancy)) %>%
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
    dplyr::anti_join(dplyr::select(id_NA, -discrepancy)) %>%
    dplyr::anti_join(dplyr::select(exact_dups,
                                   -c(discrepancy, `exact dup cnt`))
    ) %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(id_cols))) %>%
    dplyr::mutate(`ID dup cnt` = dplyr::row_number(),
                  .after = id_cols[length(id_cols)]) %>%
    dplyr::filter(max(`ID dup cnt`, na.rm = T) > 1) %>%
    dplyr::mutate(`ID dup cnt` = max(`ID dup cnt`, na.rm = T)) %>%
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
    dplyr::anti_join(dplyr::select(id_NA, -discrepancy)) %>%
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
#' @param df1_exact_dups,df2_eact_dups an `exact_dups` list element from a
#' [find_dups()] output, for `df1` and `df2`, respectively
#' @inheritParams insert_dummy_cols
#' @param tolerance amount by which numeric values can vary and still be
#'  considered equal (helps reduce machine rounding errors false positive
#'  differences)
#'
#' @returns a list returned by [compareDF::compare_df()]
#'
compare_dfx <- function(df1_no_dups,
                        df2_no_dups,
                        df1_exact_dups,
                        df2_exact_dups,
                        id_cols,
                        tolerance = 0.00001) {

  # make round_output_to match the tolerance
  if (tolerance > 1) {
    rnd <- 1
  } else {
    tol_chr <- as.character(tolerance)
    tol_chr <- stringr::str_replace(tol_chr, "^\\.", "0.")
    rnd <- nchar(tol_chr) - 2
  }

  comp <- compareDF::compare_df(
    df1_no_dups,
    df2_no_dups,
    group_col = id_cols,
    tolerance_type = "difference",
    tolerance = tolerance,
    stop_on_error = FALSE,
    keep_unchanged_rows = TRUE,
    round_output_to = rnd
  )

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
  comp$comparison_df <- comp$comparison_df %>%
    dplyr::left_join(dup_cnt, by = c(id_cols, "source")) %>%
    dplyr::relocate(`exact dup cnt`, .after = discrepancy)

  return(comp)
}

#' Segregates data frame comparisons by type
#'
#' Splits the output of [compare_dfx()] in several smaller data frames
#' organized by type of change and formats it
#'
#' @inheritParams insert_dummy_cols
#' @param comparison_df the `comparison_df` element of a [compare_dfx()]
#' output
#' @returns
#'
segregate_compare <- function(comparison_df, id_cols) {

  all_dat <- comparison_df %>%
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
    dplyr::select(-n) %>%

    # check for unhandled cases
    {
      if (any(is.na(.$source))) {
        stop("There are unhandled cases in the source column marked as `NA`")
      } else {
        .
      }
    } %>%

    # mark whether a pure addition, deletion, or change
    dplyr::mutate(
      discrepancy = dplyr::case_when(
        all(chng_type == "=") ~ "matched",
        all(chng_type == "+") ~ "addition",
        all(chng_type == "-") ~ "deletion",
        all(chng_type %in% c("-", "+")) ~ "changed",
        TRUE ~ NA_character_
      ),
      .after = id_cols[length(id_cols)]
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

    dplyr::relocate(grp, .after = discrepancy) %>%
    dplyr::mutate(
      grp = dplyr::if_else(discrepancy == "changed",
                           grp,
                           NA_real_)
    ) %>%
    dplyr::rename(`change group` = grp)


  # split into additions, deletions, and changes data sets
  adds <- dplyr::filter(all_dat, stringr::str_detect(discrepancy, "addition"))
  dels <- dplyr::filter(all_dat, stringr::str_detect(discrepancy, "deletion"))
  chng <- dplyr::filter(all_dat, stringr::str_detect(discrepancy, "changed"))

  # for matched data, only keep one copy from df1
  matched <-  dplyr::filter(all_dat,
                            stringr::str_detect(discrepancy, "matched") &
                              source == "df1")

  # create alternate left/right view for changes
  chng_df2 <- chng %>%
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

  chng_df1 <- chng %>%
    dplyr::filter(source == "df1") %>%
    dplyr::select(-source) %>%
    dplyr::rename_with(
      .cols = -c(
        `change group`,
        discrepancy,
        tidyselect::all_of(id_cols)
      ),
      ~ paste0(., ".df1")
    )

  new_col_order <- colnames(chng_df2) %>%
    purrr::map(\(x) rep(x, 2)) %>%
    unlist() %>%
    purrr::imap(\(x, idx) {
      if (idx %% 2 == 1) {
        stringr::str_replace(x, "\\.df2$", ".df1")
      } else {
        x
      }
    }) %>%
    unlist()

  chng_lr <- chng_df1 %>%
    dplyr::bind_cols(chng_df2) %>%
    dplyr::select(
      tidyselect::all_of(id_cols),
      discrepancy,
      `change group`,
      tidyselect::all_of(new_col_order)
    )

  return(
    list(
      adds = adds,
      dels = dels,
      chng = chng,
      matched = matched,
      chng_lr = chng_lr
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
#' @param adds data frame of additions from `df1`
#' @param dels data frame of deletions (in `df2` but not in `df1`)
#' @param chng_lr data frame of changes in the left/right layout so that there
#'  is one row per change, not two
#' @param matched data frame of unchanged rows where duplicates have been removed
#' @param dup_list1,dup_list2 lists output from [find_dups()]
#'
#' @returns a data frame with three columns: `Rows`, `df1 data`, `df2 data`
#' @export
#'
summarize_compare_rows <- function(df1,
                                   df2,
                                   adds,
                                   dels,
                                   chng_lr,
                                   matched,
                                   dup_list1,
                                   dup_list2,
                                   id_cols) {

  all_val_dup_cnt1 <- sum(dup_list1$exact_dups$`exact dup cnt`)
  all_val_dup_cnt2 <- sum(dup_list2$exact_dups$`exact dup cnt`)

  id_dup_row_cnt1 <- dup_list1$id_dups %>%
    dplyr::select(tidyselect::all_of(id_cols), `ID dup cnt`) %>%
    dplyr::distinct(dplyr::across(tidyselect::all_of(id_cols)),
      .keep_all = T
    ) %>%
    dplyr::pull(`ID dup cnt`) %>%
    sum()

  id_dup_row_cnt2 <- dup_list2$id_dups %>%
    dplyr::select(tidyselect::all_of(id_cols), `ID dup cnt`) %>%
    dplyr::distinct(dplyr::across(tidyselect::all_of(id_cols)),
      .keep_all = T
    ) %>%
    dplyr::pull(`ID dup cnt`) %>%
    sum()

  id_na_cnt1 <- nrow(dup_list1$id_NA)
  id_na_cnt2 <- nrow(dup_list2$id_NA)

  summ_df <- tibble::tribble(
    ~`Rows`, ~`df1 data`, ~`df2 data`,
    "Total", nrow(df1), nrow(df2),
    "Added", nrow(adds), NA_real_,
    "Deleted", NA_real_, nrow(dels),
    "Changed", nrow(chng_lr), nrow(chng_lr),
    "Unchanged", nrow(matched), nrow(matched),
    "All values duplicated", all_val_dup_cnt1, all_val_dup_cnt2,
    "Only IDs duplicated", id_dup_row_cnt1, id_dup_row_cnt2,
    "`NA` ID values", id_na_cnt1, id_na_cnt2
  )

  return(summ_df)
}
