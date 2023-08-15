#' Example Data Frame 1
#'
#' An example data frame for testing compareDFx
#'
#' @format ## 'df1'
#' A data frame with 10 rows and 12 columns:
#' \describe{
#'  \item{id1}{ID column 1}
#'  \item{id2}{ID column 2}
#'  \item{num}{a numeric column}
#'  \item{char}{a character column}
#'  \item{int}{an integer column}
#'  \item{log}{a logical column}
#'  \item{date}{a Date column}
#'  \item{class_num_char}{a numeric column that corresponds to a character column in `df2`}
#'  \item{class_num_log}{a numeric column that corresponds to a logical column in `df2`}
#'  \item{dec_diff_ref}{a numeric column used to calculate `dec_diff`}
#'  \item{dec_diff}{a numeric column for testing `tolerance` sensitivity}
#'  \item{extra1}{a column in `df1` which does not exist in `df2`}
#' }
"df1"


#' Example Data Frame 2
#'
#' An example data frame for testing compareDFx
#'
#' @format ## 'df2'
#' A data frame with 8 rows and 12 columns:
#' \describe{
#'  \item{id1}{ID column 1}
#'  \item{id2}{ID column 2}
#'  \item{num}{a numeric column}
#'  \item{char}{a character column}
#'  \item{int}{an integer column}
#'  \item{log}{a logical column}
#'  \item{date}{a Date column}
#'  \item{class_num_char}{a character column that corresponds to a numeric column in `df1`}
#'  \item{class_num_log}{a logical column that corresponds to a numeric column in `df1`}
#'  \item{dec_diff_ref}{a numeric column used to calculate `dec_diff`}
#'  \item{dec_diff}{a numeric column for testing `tolerance` sensitivity}
#'  \item{extra2}{a column in `df2` which does not exist in `df1`}
#' }
"df2"
