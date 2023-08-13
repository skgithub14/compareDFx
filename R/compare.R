#' Compare column names of two data frames to find differences
#'
#' Indicates if two data frames have the same column names or not and what the
#' differences are.
#'
#' @param c1,c2 data frames to compare
#'
#' @returns a named list with 4 elements:
#'  * `same`: a logical indicating whether `c1` and `c2` have the same columns
#'  * `both`: a character vector with the column names in both `c1` and `c2`
#'  * `c1_only`: a character vector with the column names only in `c1`
#'  * `c2_only`: a character vector with the column names only in `c2`
#'
compare_cols <- function(c1, c2) {

  both <- intersect(colnames(c1), colnames(c2))
  c1_only <- setdiff(colnames(c1), colnames(c2))
  c2_only <- setdiff(colnames(c2), colnames(c1))

  if (length(c1_only) == 0 & length(c2_only) == 0){
    same <- TRUE
  } else {
    same <- FALSE
  }

  return(
    list(
      same = same,
      both = both,
      c1_only = c1_only,
      c2_only = c2_only
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
#'  * `c1`: the modified `c1` data frame
#'  * `c2`: the modified `c2` data frame
#'
insert_dummy_cols <- function (c1, c2, cc_out, id_cols) {

  # do nothing if they already have the same columns
  if (cc_out$same) {
    return(
      list(
        c1 = c1,
        c2 = c2)
    )
  }

  # create missing columns with correct classes via utility function
  make_cols <- function (ref_df, work_df, make_cols) {
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

  c1 <- make_cols(ref_df = c2, work_df = c1, make_cols = cc_out$c2_only)
  c2 <- make_cols(ref_df = c1, work_df = c2, make_cols = cc_out$c1_only)

  return(
    list(
      c1 = c1,
      c2 = c2)
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
#'  * `c1`: a modified data frame `c1`
#'  * `c2`: a modified data frame `c2`
#'
standardize_col_order <- function (c1, c2, id_cols) {

  not_id_cols <- setdiff(colnames(c1), id_cols)

  c1 <- dplyr::select(c1,
                      tidyselect::all_of(id_cols),
                      tidyselect::all_of(not_id_cols))

  c2 <- dplyr::select(c2, tidyselect::all_of(colnames(c1)))

  return(
    list(
      c1 = c1,
      c2 = c2)
  )
}

#' Separate duplicated data frame rows from rows with no duplicates
#'
#' Separates a data frame into sub-data frames based on types of row duplication
#' or lack thereof. Also identifies `NA` values in `id_cols`.
#'
#' @param df a data frame with or without duplicated rows
#' @param id_cols a character vector of the column names that form a unique ID
#'
#' @returns a names list with four elements:
#'  * `every_dups`: rows which were duplicated for every value, there will be
#'   one distinct row in `every_dups` for each case and the column `n` will
#'   list how many occurences of the row were in `df`.
#'  * `id_dups`: rows which were duplicated based on `id_cols` but had other
#'   values that differed, the column `n` contains how many ID duplicates of
#'   one ID exist in `df`
#'  * `id_NA`: rows which had an `NA` value in a `id_cols` column
#'  * `no_dups`: this is `df` with all `id_dups` row removed and only one copy
#'   of each `every_dups` row.
#'
find_dups <- function (df, id_cols) {

  # duplicates that match on every column
  every_dups <- df %>%
    dplyr::group_by(dplyr::across(tidyselect::everything())) %>%
    dplyr::mutate(n = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(n > 1) %>%
    dplyr::slice_max(n) %>%
    dplyr::relocate(n, .after = id_cols[length(id_cols)]) %>%
    dplyr::distinct()

  # duplicates that match only by the ID columns but have other differences
  id_dups <- df %>%
    dplyr::anti_join(dplyr::select(every_dups, -c(n))) %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(id_cols))) %>%
    dplyr::mutate(n = dplyr::row_number()) %>%
    dplyr::filter(max(n, na.rm= T) > 1) %>%
    dplyr::mutate(n = max(n, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::relocate(n, .after = id_cols[length(id_cols)])

  # check for id columns with NA values
  id_NA <- df %>%
    dplyr::filter(dplyr::if_any(.cols = tidyselect::all_of(id_cols),
                                ~ is.na(.)))

  # data frame with duplicates removed:
  #  * ID duplicates are removed because they have other discrepancies
  #  * Exact match duplicates are reduced to one instance
  #  * ids with NA values are retained
  no_dups <- df %>%
    dplyr::anti_join(dplyr::select(id_dups, -c(n)), by = id_cols) %>%
    dplyr::distinct()

  return(
    list(
      every_dups = every_dups,
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
#' Rows that were duplicated in `c1` or `c2` should be removed prior to calling
#' this function
#'
#' @inheritParams compare_cols
#' @inheritParams insert_dummy_cols
#' @param tolerance amount by which numeric values can vary and still be
#'  considered equal (helps reduce machine rounding errors false positive
#'  differences)
#'
#' @returns a list returned by [compareDF::compare_df()]
#'
compareDF_custom <- function (c1, c2, id_cols, tolerance = 0.00001) {

  # make round_output_to match the tolerance
  if (tolerance > 1) {
    rnd <- 1
  } else {
    tol_chr <- as.character(tolerance)
    tol_chr <- stringr::str_replace(tol_chr, "^\\.", "0.")
    rnd <- nchar(tol_chr) - 2
  }

  comp <- compareDF::compare_df(c1,
                                c2,
                                group_col = id_cols,
                                tolerance_type = "difference",
                                tolerance = tolerance,
                                stop_on_error = FALSE,
                                keep_unchanged_rows = TRUE,
                                round_output_to = rnd)
  return(comp)
}

#' Segregates data frame comparisons by type
#'
#' Splits the output of [compareDF_custom()] in several smaller data frames
#' organized by type of change and formats it
#'
#' @param comp_comparison_df the `comparison_df` element of a
#' [compareDF_custom()] output
#' @returns
#'
segregate_compare <- function (comp_comparison_df) {

  all_dat <- comp_comparison_df %>%
    dplyr::group_by(grp) %>%
    dplyr::mutate(n = dplyr::row_number(), .after = "grp") %>%

    # mark which row came from what data frame
    dplyr::mutate(
      data_from = dplyr::case_when(
        all(chng_type == "+") ~ "new",
        all(chng_type == "-") ~ "old",
        chng_type == "=" & n == 1 ~ "new",
        chng_type == "=" & n == 2 ~ "old",
        chng_type == "+" & max(n, na.rm = T) == 2 ~ "new",
        chng_type == "-" & max(n, na.rm = T) == 2 ~ "old",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::select(-n) %>%
    dplyr::relocate(data_from, .after = chng_type) %>%

    # check for unhandled cases
    {
      if (any(is.na(.$data_from))) {
        stop("There are unhandled cases in the data_from column marked as `NA`")
      } else {
        .
      }
    } %>%

    # mark whether a pure addition, deletion, or change
    dplyr::mutate(
      change_type = dplyr::case_when(
        all(chng_type == "=") ~ "unchanged",
        all(chng_type == "+") ~ "addition",
        all(chng_type == "-") ~ "deletion",
        all(chng_type %in% c("-", "+")) ~ "change",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::relocate(change_type, .after = data_from) %>%
    dplyr::select(-chng_type) %>%

    # check for unhandled cases
    {
      if (any(is.na(.$change_type))) {
        stop("There are unhandled cases in the change_type column marked as `NA`")
      } else {
        .
      }
    } %>%

    dplyr::ungroup()

  # split into additions, deletions, and changes data sets
  adds <- dplyr::filter(all_dat, change_type == "addition")
  dels <- dplyr::filter(all_dat, change_type == "deletion")
  chng <- dplyr::filter(all_dat, change_type == "change")
  unchng <- all_dat %>%
    dplyr::filter(change_type == "unchanged") %>%
    dplyr::select(-data_from) %>%
    dplyr::distinct()

  # create alternate left/right view for changes
  chng_old <- chng %>%
    dplyr::filter(data_from == "old") %>%
    dplyr::select(-c(data_from,
                     change_type,
                     grp,
                     tidyselect::all_of(id_cols))
    ) %>%
    dplyr::rename_with(.cols = tidyselect::everything(),
                       ~ paste0(., ".old"))

  chng_new <- chng %>%
    dplyr::filter(data_from == "new") %>%
    dplyr::select(-data_from)

  new_col_order <- colnames(chng_old) %>%
    purrr::map(\(x) rep(x, 2)) %>%
    unlist() %>%
    purrr::imap(\(x, idx) {
      if (idx %% 2 == 1){
        stringr::str_remove(x, "\\.old")
      } else {
        x
      }
    }) %>%
    unlist()

  chng_lr <- chng_new %>%
    dplyr::bind_cols(chng_old) %>%
    dplyr::select(grp,
                  change_type,
                  tidyselect::all_of(id_cols),
                  tidyselect::all_of(new_col_order))

  return(
    list(
      adds = adds,
      dels = dels,
      chng = chng,
      unchng = unchng,
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
#' @param adds data frame of additions from `c1`
#' @param dels data frame of deletions (in `c2` but not in `c1`)
#' @param chng_lr data frame of changes in the left/right layout so that there
#'  is one row per change, not two
#' @param unchng data frame of unchanged rows where duplicates have been removed
#' @param dup_list1,dup_list2 lists output from [find_dups()]
#'
#' @returns a data frame with three columns: `Rows`, `New data`, `Old data`
#' @export
#'
summarize_compare_rows <- function (c1,
                                    c2,
                                    adds,
                                    dels,
                                    chng_lr,
                                    unchng,
                                    dup_list1,
                                    dup_list2,
                                    id_cols) {

  all_val_dup_cnt1 <- sum(dup_list1$every_dups$n)
  all_val_dup_cnt2 <- sum(dup_list2$every_dups$n)

  id_dup_row_cnt1 <- dup_list1$id_dups %>%
    dplyr::select(tidyselect::all_of(id_cols), n) %>%
    dplyr::distinct(dplyr::across(tidyselect::all_of(id_cols)),
                    .keep_all = T) %>%
    dplyr::pull(n) %>%
    sum()

  id_dup_row_cnt2 <- dup_list2$id_dups %>%
    dplyr::select(tidyselect::all_of(id_cols), n) %>%
    dplyr::distinct(dplyr::across(tidyselect::all_of(id_cols)),
                    .keep_all = T) %>%
    dplyr::pull(n) %>%
    sum()

  id_na_cnt1 <- nrow(dup_list1$id_NA)
  id_na_cnt2 <- nrow(dup_list2$id_NA)

  summ_df <- tibble::tribble(
    ~ `Rows`               , ~ `New data`      , ~ `Old data`,
    "Total"                , nrow(c1)          , nrow(c2)          ,
    "Added"                , nrow(adds)        , NA_real_          ,
    "Deleted"              , NA_real_          , nrow(dels)        ,
    "Changed"              , nrow(chng_lr)     , nrow(chng_lr)     ,
    "Unchanged"            , nrow(unchng)      , nrow(unchng)      ,
    "All values duplicated", all_val_dup_cnt1  , all_val_dup_cnt2  ,
    "Only IDs duplicated"  , id_dup_row_cnt1   , id_dup_row_cnt2   ,
    "`NA` ID values"       , id_na_cnt1        , id_na_cnt2
  )

  return(summ_df)
}

