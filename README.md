
<!-- README.md is generated from README.Rmd. Please edit that file -->

# compareDFx <a href="https://skgithub14.github.io/compareDFx/"><img src="man/figures/logo.png" align="right" height="120" alt="compareDFx website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/skgithub14/compareDFx/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/skgithub14/compareDFx/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/skgithub14/compareDFx/branch/master/graph/badge.svg)](https://app.codecov.io/gh/skgithub14/compareDFx?branch=master)
<!-- badges: end -->

{compareDFx} compares two data frames with detailed reporting natively
in R or in MS Excel. The comparison analysis shows changes and errors by
row and column and provides summary statistics. {compareDFx} can also
compare data frames with different columns and column classes. The MS
Excel report is a multi-tab workbook that color codes additions,
deletions, and changes which is ideal for R users and non-R users alike.

## Installation

You can install the development version of {compareDFx} like so:

``` r
# install.package("devtools")
install_github("skgithub14/compareDFx")
```

## Comparing two data frames using {compareDFx}

Using the two example data frames included with the package, `df1` and
`df2`, we will walk through the {compareDFx} workflow.

``` r
library(compareDFx)

# example data, df1 
knitr::kable(compareDFx::df1)
```

| id1 | id2 | num | char | int | log   | date       | class_num_char | class_num_log | dec_diff_ref | dec_diff |
|----:|:----|----:|:-----|----:|:------|:-----------|---------------:|--------------:|-------------:|---------:|
|   1 | A   |   1 | a    |   1 | NA    | 2023-01-01 |            100 |            NA |            1 |        1 |
|   2 | B   |   2 | NA   |  NA | TRUE  | NA         |            100 |            NA |            1 |        1 |
|   3 | C   |  NA | c    |   2 | FALSE | 2023-01-03 |            100 |            NA |            1 |        1 |
|   4 | D   |   4 | d    |   3 | NA    | 2023-01-04 |            100 |            NA |            1 |        1 |
|   5 | E   |   5 | e    |   5 | FALSE | 2023-01-05 |            100 |            NA |            1 |        1 |
|   5 | E   |   5 | g    |   5 | FALSE | 2023-01-05 |            100 |            NA |            1 |        1 |
|   5 | Z   |   5 | e    |   5 | FALSE | 2023-01-05 |            100 |            NA |            1 |        1 |
|   5 | Z   |   5 | e    |   5 | FALSE | 2023-01-05 |            100 |            NA |            1 |        1 |
|   5 | NA  |   5 | g    |   5 | FALSE | 2023-01-05 |            100 |            NA |            1 |        1 |
|   6 | M   |   5 | e    |   5 | FALSE | 2023-01-05 |            100 |            NA |            1 |        1 |

``` r
# example data, df2
knitr::kable(compareDFx::df2)
```

| id1 | id2 | num | char | int | log   | date       | class_num_char | class_num_log | dec_diff_ref | dec_diff |
|----:|:----|----:|:-----|----:|:------|:-----------|:---------------|:--------------|-------------:|---------:|
|   1 | A   |   1 | a    |   1 | NA    | 2023-01-01 | 100            | NA            |        0e+00 | 1.000000 |
|   1 | A   |   1 | a    |   1 | NA    | 2023-01-01 | 100            | NA            |        0e+00 | 1.000000 |
|   1 | A   |   1 | a    |   1 | NA    | 2023-01-01 | 100            | NA            |        0e+00 | 1.000000 |
|   2 | B   |   2 | NA   |  NA | TRUE  | NA         | 100            | NA            |       -1e-04 | 1.000100 |
|   3 | C   |  NA | c    |   2 | FALSE | 2023-01-03 | 100            | NA            |       -1e-05 | 1.000010 |
|   4 | D   |   4 | d    |   3 | NA    | 2023-01-04 | 100            | NA            |       -1e-06 | 1.000001 |
|   6 | F   |   6 | f    |   6 | TRUE  | 2023-01-06 | 100            | NA            |        0e+00 | 1.000000 |
|   6 | M   |   5 | e    |   5 | FALSE | 2023-01-05 | 100            | NA            |        1e+00 | 1.000000 |

Next, we need to identify the columns which collectively make a unique
ID for each row, then we can run the comparison.

``` r
id_cols <- c("id1", "id2")
comparison <- get_comparison(df1, df2, id_cols)
```

The output of `get_comparison()` is a list with elements:

``` r
names(comparison)
#>  [1] "col_summary_simple"     "col_summary_by_col"     "row_summary"           
#>  [4] "all"                    "all_index_by_col"       "matched"               
#>  [7] "adds"                   "dels"                   "changed"               
#> [10] "change_index_by_col"    "changed_lr"             "change_index_by_col_lr"
#> [13] "exact_dups"             "id_dups"                "id_NA"                 
#> [16] "df1"                    "df2"                    "id_cols"               
#> [19] "cc_out"
```

See the `get_comparison()` documentation for details on each list
element. Below we will view the most important ones.

Summary statistics are provided by `$row_summary`,
`$col_summary_simple`, and `$col_summary_by_col`.

``` r
knitr::kable(comparison$row_summary)
```

| Rows               | df1 | df2 |
|:-------------------|----:|----:|
| Total              |  10 |   8 |
| Added              |   1 |  NA |
| Deleted            |  NA |   1 |
| Changed            |   4 |   4 |
| Unchanged          |   1 |   1 |
| Exact duplicate    |   2 |   3 |
| ID only duplicates |   2 |   0 |
| `NA` ID values     |   1 |   0 |

``` r
knitr::kable(comparison$col_summary_simple)
```

| Columns        | df1 | df2 |
|:---------------|----:|----:|
| Total          |  11 |  11 |
| Changed        |   2 |   2 |
| Mismatch class |   2 |   2 |

``` r
knitr::kable(comparison$col_summary_by_col)
```

| Column         | In df1 | In df2 | Changes | Mismatched class | df1 class | df2 class |
|:---------------|:-------|:-------|--------:|:-----------------|:----------|:----------|
| id1            | TRUE   | TRUE   |       0 | FALSE            | numeric   | numeric   |
| id2            | TRUE   | TRUE   |       0 | FALSE            | character | character |
| num            | TRUE   | TRUE   |       0 | FALSE            | numeric   | numeric   |
| char           | TRUE   | TRUE   |       0 | FALSE            | character | character |
| int            | TRUE   | TRUE   |       0 | FALSE            | integer   | integer   |
| log            | TRUE   | TRUE   |       0 | FALSE            | logical   | logical   |
| date           | TRUE   | TRUE   |       0 | FALSE            | Date      | Date      |
| class_num_char | TRUE   | TRUE   |       0 | TRUE             | numeric   | character |
| class_num_log  | TRUE   | TRUE   |       0 | TRUE             | numeric   | logical   |
| dec_diff_ref   | TRUE   | TRUE   |       4 | FALSE            | numeric   | numeric   |
| dec_diff       | TRUE   | TRUE   |       2 | FALSE            | numeric   | numeric   |

`$all` shows all of the compared data from `df1` and `df2`.

``` r
knitr::kable(comparison$all)
```

| source | id1 | id2 | discrepancy                        | change group | exact dup cnt | ID dup cnt | num | char | int | log   | date       | class_num_char | class_num_log | dec_diff_ref | dec_diff |
|:-------|----:|:----|:-----------------------------------|-------------:|--------------:|-----------:|----:|:-----|----:|:------|:-----------|:---------------|--------------:|-------------:|---------:|
| df1    |   1 | A   | changed                            |            1 |            NA |         NA |   1 | a    |   1 | NA    | 2023-01-01 | 100            |            NA |        1e+00 | 1.000000 |
| df2    |   1 | A   | changed, exact duplicate           |            1 |             3 |         NA |   1 | a    |   1 | NA    | 2023-01-01 | 100            |            NA |        0e+00 | 1.000000 |
| df1    |   2 | B   | changed                            |            2 |            NA |         NA |   2 | NA   |  NA | TRUE  | NA         | 100            |            NA |        1e+00 | 1.000000 |
| df2    |   2 | B   | changed                            |            2 |            NA |         NA |   2 | NA   |  NA | TRUE  | NA         | 100            |            NA |       -1e-04 | 1.000100 |
| df1    |   3 | C   | changed                            |            3 |            NA |         NA |  NA | c    |   2 | FALSE | 2023-01-03 | 100            |            NA |        1e+00 | 1.000000 |
| df2    |   3 | C   | changed                            |            3 |            NA |         NA |  NA | c    |   2 | FALSE | 2023-01-03 | 100            |            NA |       -1e-05 | 1.000010 |
| df1    |   4 | D   | changed                            |            4 |            NA |         NA |   4 | d    |   3 | NA    | 2023-01-04 | 100            |            NA |        1e+00 | 1.000000 |
| df2    |   4 | D   | changed                            |            4 |            NA |         NA |   4 | d    |   3 | NA    | 2023-01-04 | 100            |            NA |       -1e-06 | 1.000001 |
| df1    |   5 | E   | ID duplicate (not exact duplicate) |           NA |            NA |          2 |   5 | e    |   5 | FALSE | 2023-01-05 | 100            |            NA |        1e+00 | 1.000000 |
| df1    |   5 | E   | ID duplicate (not exact duplicate) |           NA |            NA |          2 |   5 | g    |   5 | FALSE | 2023-01-05 | 100            |            NA |        1e+00 | 1.000000 |
| df1    |   5 | Z   | addition, exact duplicate          |           NA |             2 |         NA |   5 | e    |   5 | FALSE | 2023-01-05 | 100            |            NA |        1e+00 | 1.000000 |
| df1    |   5 | NA  | ID contains `NA`                   |           NA |            NA |         NA |   5 | g    |   5 | FALSE | 2023-01-05 | 100            |            NA |        1e+00 | 1.000000 |
| df2    |   6 | F   | deletion                           |           NA |            NA |         NA |   6 | f    |   6 | TRUE  | 2023-01-06 | 100            |            NA |        0e+00 | 1.000000 |
| df1    |   6 | M   | matched                            |           NA |            NA |         NA |   5 | e    |   5 | FALSE | 2023-01-05 | 100            |            NA |        1e+00 | 1.000000 |

The data frame shown above contains special columns that provide
information on the comparison for each record:

- `source`: whether the data in the row came from `df1` or `df2`

- `discrepancy`: the comparison status, which can take one or more of
  the following values:

  - `"matched"`: a row that exactly matched between `df1` and `df2` (for
    `matched` rows, only the `df1` copy is shown)

  - `"addition"`: a row that only existed in `df1`

  - `"deletion"`: a row that only existed in `df2`

  - `"changed"`: a row where the `id_cols` matched between between `df1`
    and `df2` but which had a least one other column with a change

  - `"exact duplicate"`: a row with multiple occurrences in either `df1`
    or `df2`. “Exact” means that every value in the row matched
    perfectly to its duplicate copies. Exact duplicates can also be
    marked as `"matched"`, `"addition"`, `"deletion"`, or `"changed"`.

  - `"ID duplicate (not exact duplicate)"`: a row that had multiple
    occurrences of the same combination of `id_cols` in either `df1` or
    `df2` but for which there was not an exact duplicate (meaning at
    least one non-ID value in the row was different). An ID duplicate
    cannot be marked as `"matched"`, `"addition"`, `"deletion"`, or
    `"changed"` because there will be multiple value matches when
    comparing `df1` and `df2`. The user should fix these errors and
    re-run the comparison.

  - `"ID contains`NA`"`: a row in either `df1` or `df2` which had an
    `NA` value in one or more `id_cols`. The comparison procedure
    requires non-`NA` values for all `id_cols` therefore these rows will
    not be marked as `"matched"`, `"addition"`, `"deletion"`, or
    `"changed"`. The user should fix these errors and re-run the
    comparison.

- `change group`: if `discrepancy` contains `"changed"` this column will
  be populated with an arbitrary number that links changed records
  between `df1` and `df2`

- `exact dup cnt`: if `discrepancy` contains `"exact duplicate"` this
  column give the number of exact duplicates in `df1` or `df2`

- `ID dup cnt`: if `discrepancy` contains
  `"ID duplicate (not exact duplicate)"` this column gives the number of
  ID duplicates in `df1` or `df2`

Finally, we can produce a multi-tab, color-coded MS Excel report of our
comparison using `create_comparison_excel()`:

``` r
dir <- system.file("extdata", package = "compareDFx")
create_comparison_excel(comparison, 
                        path = file.path(dir, "comparison_report.xlsx"))
```
