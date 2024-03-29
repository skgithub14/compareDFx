% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_comparison.R
\name{get_comparison}
\alias{get_comparison}
\title{Compare two data frames}
\usage{
get_comparison(df1, df2, id_cols, tolerance = 1e-05)
}
\arguments{
\item{df1, df2}{data frames to compare}

\item{id_cols}{the column names in \code{df1} and \code{df2} that make up a unique ID}

\item{tolerance}{the amount by which two numbers can differ to be considered
equal, default is \code{0.00001}}
}
\value{
a named list with elements:
\itemize{
\item \code{col_summary_simple}: summary statistics for columns
\item \code{col_summary_by_col}: summary statistics by column
\item \code{row_summary}: summary statistics for rows
\item \code{all_pivoted}: comparison data displayed where each value in \code{df1} is
shown directly to the left of its \code{df2} counterpart (a pivoted view) with
comparison annotation columns
\item \code{all_tb}: comparison data displayed where each row in \code{df1} is shown
directly above its \code{df2} counterpart (a top-bottom view) with comparison
annotation columns
\item \code{all_lr}: comparison data displayed where each column in \code{df1} is shown
to the left of its \code{df2} counterpart (a left-right view) with comparison
annotation columns
\item \code{all_tb_change_indices}: a named list where the names are the data columns
in \code{all_tb} and the elements are numeric vectors of the row indices that
changed between \code{df1} and \code{df2} in a column
\item \code{all_lr_change_indices}: a named list where the names are the data columns
in \code{all_lr} and the elements are numeric vectors of the row indices that
changed between \code{df1} and \code{df2} in a column
\item \code{id_cols}: the columns in \code{df1} and \code{df2} that form a unique row ID
\item \code{cc_out}: a name list with four elements:
\itemize{
\item \code{same}: a logical indicating whether the column names in \code{df1} and \code{df2}
are the same
\item \code{both}: a character vector of column names that are in both \code{df1} and
\code{df2}
\item \code{df1_only}: character vector of column names that are in \code{df1} but not
\code{df2}
\item \code{df2_only}: character vector of column names that are in \code{df2} but not
\code{df1}
}
\item \code{df1}: the raw data from \code{df1}
\item \code{df2}: the raw data from \code{df2}
}
}
\description{
Generates a detailed comparison report of two data frames
}
\examples{
id_cols <- c("id1", "id2")
comparison <- get_comparison(compareDFx::df1, compareDFx::df2, id_cols)

}
