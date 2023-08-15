
<!-- README.md is generated from README.Rmd. Please edit that file -->

# compareDFx

<!-- badges: start -->

[![R-CMD-check](https://github.com/skgithub14/compareDFx/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/skgithub14/compareDFx/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/skgithub14/compareDFx/branch/master/graph/badge.svg)](https://app.codecov.io/gh/skgithub14/compareDFx?branch=master)
<!-- badges: end -->

The goal of compareDFx is to compare two different versions of the same
data frame using a deatiled MS Excel report.

## Installation

You can install the development version of compareDFx like so:

``` r
# install.package("devtools")
install_github("skgithub14/compareDFx")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(compareDFx)

# view the raw data
compareDFx::df1
#>    id1  id2 num char int   log       date class_num_char class_num_log
#> 1    1    A   1    a   1    NA 2023-01-01            100            NA
#> 2    2    B   2 <NA>  NA  TRUE       <NA>            100            NA
#> 3    3    C  NA    c   2 FALSE 2023-01-03            100            NA
#> 4    4    D   4    d   3    NA 2023-01-04            100            NA
#> 5    5    E   5    e   5 FALSE 2023-01-05            100            NA
#> 6    5    E   5    g   5 FALSE 2023-01-05            100            NA
#> 7    5    Z   5    e   5 FALSE 2023-01-05            100            NA
#> 8    5    Z   5    e   5 FALSE 2023-01-05            100            NA
#> 9    5 <NA>   5    g   5 FALSE 2023-01-05            100            NA
#> 10   6    M   5    e   5 FALSE 2023-01-05            100            NA
#>    dec_diff_ref dec_diff
#> 1             1        1
#> 2             1        1
#> 3             1        1
#> 4             1        1
#> 5             1        1
#> 6             1        1
#> 7             1        1
#> 8             1        1
#> 9             1        1
#> 10            1        1
compareDFx::df2
#>   id1 id2 num char int   log       date class_num_char class_num_log
#> 1   1   A   1    a   1    NA 2023-01-01            100            NA
#> 2   1   A   1    a   1    NA 2023-01-01            100            NA
#> 3   1   A   1    a   1    NA 2023-01-01            100            NA
#> 4   2   B   2 <NA>  NA  TRUE       <NA>            100            NA
#> 5   3   C  NA    c   2 FALSE 2023-01-03            100            NA
#> 6   4   D   4    d   3    NA 2023-01-04            100            NA
#> 7   6   F   6    f   6  TRUE 2023-01-06            100            NA
#> 8   6   M   5    e   5 FALSE 2023-01-05            100            NA
#>   dec_diff_ref dec_diff
#> 1        0e+00 1.000000
#> 2        0e+00 1.000000
#> 3        0e+00 1.000000
#> 4       -1e-04 1.000100
#> 5       -1e-05 1.000010
#> 6       -1e-06 1.000001
#> 7        0e+00 1.000000
#> 8        1e+00 1.000000
```

``` r
# columns that collectively make a unique ID for each row in df1 or df2
id_cols <- c("id1", "id2")
```

``` r
comparison <- get_comparison(df1, df2, id_cols)
#> Joining with `by = join_by(id1, id2, num, char, int, log, date, class_num_char,
#> class_num_log, dec_diff_ref, dec_diff)`
#> Joining with `by = join_by(id1, id2, num, char, int, log, date, class_num_char,
#> class_num_log, dec_diff_ref, dec_diff)`
#> Joining with `by = join_by(id1, id2, num, char, int, log, date, class_num_char,
#> class_num_log, dec_diff_ref, dec_diff)`
#> Joining with `by = join_by(id1, id2, num, char, int, log, date, class_num_char,
#> class_num_log, dec_diff_ref, dec_diff)`
#> Joining with `by = join_by(id1, id2, num, char, int, log, date, class_num_char,
#> class_num_log, dec_diff_ref, dec_diff)`
#> Joining with `by = join_by(id1, id2, num, char, int, log, date, class_num_char,
#> class_num_log, dec_diff_ref, dec_diff)`
#> Joining with `by = join_by(id1, id2, num, char, int, log, date, class_num_char,
#> class_num_log, dec_diff_ref, dec_diff)`
#> Warning: There was 1 warning in `dplyr::mutate()`.
#> ℹ In argument: `ID dup cnt = max(`ID dup cnt`, na.rm = T)`.
#> Caused by warning in `max()`:
#> ! no non-missing arguments to max; returning -Inf
#> Joining with `by = join_by(id1, id2, num, char, int, log, date, class_num_char,
#> class_num_log, dec_diff_ref, dec_diff)`
#> Warning: Unknown columns: `exact dup cnt`
#> Warning: Unknown columns: `exact dup cnt`
#> Found Date columns! Will be casted to character for comparison!
#> Found Date columns! Will be casted to character for comparison!
#> Grouping columns
#> Creating comparison table...
```

``` r
comparison$row_summary
#> # A tibble: 8 × 3
#>   Rows                 df1   df2
#>   <chr>              <dbl> <dbl>
#> 1 Total                 10     8
#> 2 Added                  1    NA
#> 3 Deleted               NA     1
#> 4 Changed                4     4
#> 5 Unchanged              1     1
#> 6 Exact duplicate        2     3
#> 7 ID only duplicates     2     0
#> 8 `NA` ID values         1     0
```

``` r
comparison$col_summary_simple
#> # A tibble: 3 × 3
#>   Columns          df1   df2
#>   <chr>          <dbl> <dbl>
#> 1 Total             11    11
#> 2 Changed            2     2
#> 3 Mismatch class     2     2
```

``` r
comparison$col_summary_by_col
#> # A tibble: 11 × 7
#>    Column   `In df1` `In df2` Changes `Mismatched class` `df1 class` `df2 class`
#>    <chr>    <lgl>    <lgl>      <int> <lgl>              <chr>       <chr>      
#>  1 id1      TRUE     TRUE           0 FALSE              numeric     numeric    
#>  2 id2      TRUE     TRUE           0 FALSE              character   character  
#>  3 num      TRUE     TRUE           0 FALSE              numeric     numeric    
#>  4 char     TRUE     TRUE           0 FALSE              character   character  
#>  5 int      TRUE     TRUE           0 FALSE              integer     integer    
#>  6 log      TRUE     TRUE           0 FALSE              logical     logical    
#>  7 date     TRUE     TRUE           0 FALSE              Date        Date       
#>  8 class_n… TRUE     TRUE           0 TRUE               numeric     character  
#>  9 class_n… TRUE     TRUE           0 TRUE               numeric     logical    
#> 10 dec_dif… TRUE     TRUE           4 FALSE              numeric     numeric    
#> 11 dec_diff TRUE     TRUE           2 FALSE              numeric     numeric
```

``` r
comparison$all
#> # A tibble: 14 × 16
#>    source   id1 id2   discrepancy    `change group` `exact dup cnt` `ID dup cnt`
#>    <chr>  <dbl> <chr> <chr>                   <dbl>           <int>        <dbl>
#>  1 df1        1 A     changed                     1              NA           NA
#>  2 df2        1 A     changed, exac…              1               3           NA
#>  3 df1        2 B     changed                     2              NA           NA
#>  4 df2        2 B     changed                     2              NA           NA
#>  5 df1        3 C     changed                     3              NA           NA
#>  6 df2        3 C     changed                     3              NA           NA
#>  7 df1        4 D     changed                     4              NA           NA
#>  8 df2        4 D     changed                     4              NA           NA
#>  9 df1        5 E     ID duplicate …             NA              NA            2
#> 10 df1        5 E     ID duplicate …             NA              NA            2
#> 11 df1        5 Z     addition, exa…             NA               2           NA
#> 12 df1        5 <NA>  ID contains `…             NA              NA           NA
#> 13 df2        6 F     deletion                   NA              NA           NA
#> 14 df1        6 M     matched                    NA              NA           NA
#> # ℹ 9 more variables: num <dbl>, char <chr>, int <int>, log <lgl>, date <chr>,
#> #   class_num_char <chr>, class_num_log <dbl>, dec_diff_ref <dbl>,
#> #   dec_diff <dbl>
```

``` r
tmp_dir <- tempdir()
create_comparison_excel(comparison, path = file.path(tmp_dir, "comparison_report.xlsx"))
```
