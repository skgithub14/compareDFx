
<!-- README.md is generated from README.Rmd. Please edit that file -->

# compareDFx <a href="https://skgithub14.github.io/compareDFx/"><img src="man/figures/logo.png" align="right" height="139" alt="compareDFx website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/skgithub14/compareDFx/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/skgithub14/compareDFx/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/skgithub14/compareDFx/branch/master/graph/badge.svg)](https://app.codecov.io/gh/skgithub14/compareDFx?branch=master)
<!-- badges: end -->

{compareDFx} compares two data frames with detailed reporting natively
in R or in MS Excel. The MS Excel report is a multi-tab workbook that
color codes additions, deletions, and changes which is ideal for R users
and non-R users alike and makes identifying data issues and version
differences quick and easy.

![Screen shot from ‘data top-bottom’ tab of
comparison_report.xlsx](https://github.com/skgithub14/compareDFx/raw/master/inst/extdata/report_top_bottom.png)

## Installation

You can install the development version of {compareDFx} like so:

``` r
# install.package("devtools")
install_github("skgithub14/compareDFx")
```

<br>

------------------------------------------------------------------------

## Comparing two data frames using {compareDFx}

Let’s assume that `df1` and `df2` are two versions of the same data set,
where `df1` is the newer version. We want to know exactly which values
changed. Using the two example data frames included with {compareDFx},
`df1` and `df2`, we will walk through the {compareDFx} workflow.

### Creating the report

We will start by loading the {compareDFx} package and defining our data
frames:

``` r
library(compareDFx)
df1 <- compareDFx::df1
df2 <- compareDFx::df2
```

<details>
<summary>
View the example data
</summary>

``` r
# example data, df1 
knitr::kable(df1)
```

| id1 | id2 | num | char | int | log   | date       | class_num_char | class_num_log | dec_diff_ref | dec_diff | extra1 |
|----:|:----|----:|:-----|----:|:------|:-----------|---------------:|--------------:|-------------:|---------:|:-------|
|   1 | A   |   1 | a    |   1 | NA    | 2023-01-01 |            100 |            NA |            1 |        1 | extra1 |
|   2 | B   |   2 | NA   |  NA | TRUE  | NA         |            100 |            NA |            1 |        1 | extra1 |
|   3 | C   |  NA | c    |   2 | FALSE | 2023-01-03 |            100 |            NA |            1 |        1 | extra1 |
|   4 | D   |   4 | d    |   3 | NA    | 2023-01-04 |            100 |            NA |            1 |        1 | extra1 |
|   5 | E   |   5 | e    |   5 | FALSE | 2023-01-05 |            100 |            NA |            1 |        1 | extra1 |
|   5 | E   |   5 | g    |   5 | FALSE | 2023-01-05 |            100 |            NA |            1 |        1 | extra1 |
|   5 | Z   |   5 | e    |   5 | FALSE | 2023-01-05 |            100 |            NA |            1 |        1 | extra1 |
|   5 | Z   |   5 | e    |   5 | FALSE | 2023-01-05 |            100 |            NA |            1 |        1 | extra1 |
|   5 | NA  |   5 | g    |   5 | FALSE | 2023-01-05 |            100 |            NA |            1 |        1 | extra1 |
|   6 | M   |   5 | e    |   5 | FALSE | 2023-01-05 |            100 |            NA |            1 |        1 | extra1 |

``` r
# example data, df2
knitr::kable(df2)
```

| id1 | id2 | num | char | int | log   | date       | class_num_char | class_num_log | dec_diff_ref | dec_diff | extra2 |
|----:|:----|----:|:-----|----:|:------|:-----------|:---------------|:--------------|-------------:|---------:|:-------|
|   1 | A   |   1 | a    |   1 | NA    | 2023-01-01 | 100            | NA            |        0e+00 | 1.000000 | extra2 |
|   1 | A   |   1 | a    |   1 | NA    | 2023-01-01 | 100            | NA            |        0e+00 | 1.000000 | extra2 |
|   1 | A   |   1 | a    |   1 | NA    | 2023-01-01 | 100            | NA            |        0e+00 | 1.000000 | extra2 |
|   2 | B   |   2 | NA   |  NA | TRUE  | NA         | 100            | NA            |       -1e-04 | 1.000100 | extra2 |
|   3 | C   |  NA | c    |   2 | FALSE | 2023-01-03 | 100            | NA            |       -1e-05 | 1.000010 | extra2 |
|   4 | D   |   4 | d    |   3 | NA    | 2023-01-04 | 100            | NA            |       -1e-06 | 1.000001 | extra2 |
|   6 | F   |   6 | f    |   6 | TRUE  | 2023-01-06 | 100            | NA            |        0e+00 | 1.000000 | extra2 |
|   6 | M   |   5 | e    |   5 | FALSE | 2023-01-05 | 100            | NA            |        1e+00 | 1.000000 | extra2 |

</details>

{compareDFx} uses a row’s unique ID to evaluate what changed between two
data frames. Unique IDs can consist of one or multiple columns. For
`df1` and `df2`, the columns the collectively make a unique row ID are
`id1` and `id2`. This means that in `df1` there are two columns named
`id1` and `id2` and in `df2` there are also two columns named `id1` and
`id2`.

``` r
id_cols <- c("id1", "id2")
```

Now we can run the comparison and generate the report in Excel:

``` r
# run the comparison
comparison <- get_comparison(df1, df2, id_cols)

# create the excel report
create_comparison_excel(comparison, path = "comparison_report.xlsx")
```

Now we have a multi-tab, color-coded MS Excel report with summary
statistics. On the data tabs of the report, all cells from `df1` will
have a white background and all cells from `df2` will have a grey
background. The text is colored accordingly:

- ![](https://placehold.it/150/ffffff/008000?text='Green')
  ![](https://placehold.it/150/ffffff/008000?text='text'): cell values
  in `df1` that are different from `df2`

- <span style="color:red">Red text</span>: cell values in `df2` that are
  different from `df1`

- Black text: cell values that are the unchanged between `df1` and `df2`

You can explore the file yourself here:

[Download
comparison_report.xlsx](https://github.com/skgithub14/compareDFx/raw/master/inst/extdata/comparison_report.xlsx)

As you can see, there are six tabs in the Excel workbook:

1.  summary: summary statistics

![Screen shot from ‘summary’ tab of
comparison_report.xlsx](https://github.com/skgithub14/compareDFx/raw/master/inst/extdata/summary_tab.png)

2.  summary by column: column summary statistics

![Screen shot from ‘summary by column’ tab of
comparison_report.xlsx](https://github.com/skgithub14/compareDFx/raw/master/inst/extdata/summary_by_column_tab.png)

3.  data top-bottom: the combined data displayed so that each row of
    `df1` is directly above its `df2` counterpart.

![Screen shot from ‘data top-bottom’ tab of
comparison_report.xlsx](https://github.com/skgithub14/compareDFx/raw/master/inst/extdata/report_top_bottom.png)

4.  data left-right: the same data as shown in data top-bottom, but
    where the `df1` columns are immediately to the left of their `df2`
    counterpart.

![Screen shot from ‘data left-right’ tab of
comparison_report.xlsx](https://github.com/skgithub14/compareDFx/raw/master/inst/extdata/report_left_right.png)

5.  df1: the raw data from `df1`, for reference

6.  df2: the raw data from `df2`, for reference

If you aren’t interested in the Excel report but want to work with the
report in R instead, you can use the output from `comparison` object
directly (see the documentation for `get_comparison()`).

### Understanding the report

If you look to the far left in the data tabs (data top-bottom and data
left-right), you will see some extra columns that report useful
comparison information:

- `discrepancy`: the comparison status, which can take one or more of
  the following values:

  - `"matched"`: a row that exactly matched between `df1` and `df2`

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

  - `"ID contains NA"`: a row in either `df1` or `df2` which had an `NA`
    value in one or more `id_cols`. The comparison procedure requires
    non-`NA` values for all `id_cols` therefore these rows will not be
    marked as `"matched"`, `"addition"`, `"deletion"`, or `"changed"`.
    The user should fix these errors and re-run the comparison.

- `source`: this column is only show in the data top-bottom sheet and
  tells you whether the data in the row came from `df1` or `df2`

The next set of columns are hidden by default, but can be shown by
clicking the “+” in the top left corner of the worksheet:

- `change group`: if `discrepancy` contains `"changed"` this column will
  be populated with an arbitrary number that links changed records
  between `df1` and `df2`

- `exact dup cnt`: if `discrepancy` contains `"exact duplicate"` this
  column gives the number of exact duplicates in `df1` or `df2`

- `ID dup cnt`: if `discrepancy` contains
  `"ID duplicate (not exact duplicate)"` this column gives the number of
  ID duplicates in `df1` or `df2`

### Using the report

Here is one way you can use the report that I find effective:

1.  If you have any `"ID contains NA"` values in the `discrepancy`
    column, start by fixing those in your raw data, then re-run the
    report. If these only exist in the older version of your data
    (`df2`), then maybe you don’t need to fix these because you are
    planning on replacing the old version with the new version (`df1`).

2.  If you have any `"ID duplicate (not exact duplicate)"` values in the
    discrepancy column, investigate why you have rows in `df1` and/or
    `df2` that have the same ID values but different entries in the
    other columns. Similar to the step above, if these problems are only
    in your older version (`df2`), maybe you don’t need to do anything.

3.  Filter the `discrepancy` column to find any entries containing
    `"exact duplicate"`. Depending on your project, exact duplicates
    might be okay but, you might want to check there are the correct
    number of duplicates using the `exact dup cnt` column. If exact
    duplicates are not okay, remove those from your raw data then re-run
    the report.

4.  Filter the `discrepancy` column to find any entries containing
    `"addition"` and `"deletion"`. Check that the ID values are correct.
    If the ID values are incorrect or misspelled, it is possible that
    these rows are not true additions and deletions because their ID’s
    are supposed to match but do not, in which case these rows should
    actually be categorized as `"matched"` or `"changed"`. If the ID
    values look good, confirm that the deleted rows are supposed to be
    deleted and the added rows are supposed to be added.

5.  Filter the `discrepancy` column to find any entries containing
    `"changed"` and verify that all changes are expected. Fix any “bad”
    changes in the raw data and iteratively re-run the report. In some
    cases, filtering on the `change group` column can be helpful.

6.  Filter the `discrepancy` column to find any entries containing
    `"matched"` and verify that these rows were not supposed to change
    between versions. Fix the issues and re-run the report iteratively.
