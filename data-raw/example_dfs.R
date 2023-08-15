## code to prepare `df1` dataset goes here

base <- tibble::tibble(
  id1  = 1:4,
  id2  = LETTERS[1:4],
  num  = c(1, 2, NA_real_, 4),
  char = c(letters[1], NA_character_, letters[3:4]),
  int  = as.integer(c(1, NA_integer_, 2, 3)),
  log  = c(NA, TRUE, FALSE, NA),
  date = as.Date(c("2023-01-01", NA_character_, "2023-01-03", "2023-01-04"))
)

df1 <- base %>%

  ## create ID duplicates
  dplyr::add_row(
    id1  = 5,
    id2  = "E",
    num  = 5,
    char = "e",
    int  = 5L,
    log  = FALSE,
    date = as.Date("2023-01-05")
  ) %>%
  dplyr::add_row(
    id1  = 5,
    id2  = "E",
    num  = 5,
    char = "g", # this value is different
    int  = 5L,
    log  = FALSE,
    date = as.Date("2023-01-05")
  ) %>%

  ## create exact duplicate, and row that is not in df2
  dplyr::add_row(
    id1  = 5,
    id2  = "Z",
    num  = 5,
    char = "e",
    int  = 5L,
    log  = FALSE,
    date = as.Date("2023-01-05")
  ) %>%
  dplyr::add_row(
    id1  = 5,
    id2  = "Z",
    num  = 5,
    char = "e",
    int  = 5L,
    log  = FALSE,
    date = as.Date("2023-01-05")
  ) %>%

  # add a row with a NA id value
  dplyr::add_row(
    id1  = 5,
    id2  = NA_character_,
    num  = 5,
    char = "g",
    int  = 5L,
    log  = FALSE,
    date = as.Date("2023-01-05")
  ) %>%

  # create row with exact match in df2
  dplyr::add_row(
    id1  = 6,
    id2  = "M",
    num  = 5,
    char = "e",
    int  = 5L,
    log  = FALSE,
    date = as.Date("2023-01-05")
  ) %>%

  # add columns with same names and values as df2 but with different classes
  dplyr::mutate(class_num_char = rep(100, 10)) %>%
  dplyr::mutate(class_num_log = rep(NA_real_, 10)) %>%

  # make a column to look at differences in decimal places and tolerance
  dplyr::mutate(
    dec_diff_ref = rep(1, 10),
    dec_diff = rep(1, 10),
  ) %>%

  # add a column not in df2
  dplyr::mutate(extra1 = "extra1")


df2 <- base %>%

  # add a row not in df1
  dplyr::add_row(
    id1  = 6,
    id2  = "F",
    num  = 6,
    char = "f",
    int  = 6L,
    log  = TRUE,
    date = as.Date("2023-01-06")
  ) %>%

  # create row with exact match in df1
  dplyr::add_row(
    id1  = 6,
    id2  = "M",
    num  = 5,
    char = "e",
    int  = 5L,
    log  = FALSE,
    date = as.Date("2023-01-05")
  ) %>%

  # duplicate the first row 3x
  dplyr::bind_rows(base[1, ]) %>%
  dplyr::bind_rows(base[1, ]) %>%
  dplyr::arrange(id1, id2) %>%

  # add columns with same names and values as df1 but with different classes
  dplyr::mutate(class_num_char = rep("100", 8)) %>%
  dplyr::mutate(class_num_log = rep(NA, 8)) %>%

  # make a column to look a differences in decimal places and tolerance
  dplyr::mutate(
    dec_diff_ref = c(0, 0, 0, -0.0001, -0.00001, -0.000001, 0, 1),
    dec_diff = 1 - dec_diff_ref
  ) %>%
  dplyr::mutate(dec_diff = dplyr::if_else(
    id1 == 6 & id2 == "M",
    1,
    dec_diff
  )) %>%

  # add a column not in df1
  dplyr::mutate(extra2 = "extra2")

usethis::use_data(df1, overwrite = TRUE)
usethis::use_data(df2, overwrite = TRUE)
