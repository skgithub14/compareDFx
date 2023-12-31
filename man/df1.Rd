% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{df1}
\alias{df1}
\title{Example Data Frame 1}
\format{
\subsection{'df1'}{

A data frame with 10 rows and 12 columns:
\describe{
\item{id1}{ID column 1}
\item{id2}{ID column 2}
\item{num}{a numeric column}
\item{char}{a character column}
\item{int}{an integer column}
\item{log}{a logical column}
\item{date}{a Date column}
\item{class_num_char}{a numeric column that corresponds to a character column in \code{df2}}
\item{class_num_log}{a numeric column that corresponds to a logical column in \code{df2}}
\item{dec_diff_ref}{a numeric column used to calculate \code{dec_diff}}
\item{dec_diff}{a numeric column for testing \code{tolerance} sensitivity}
\item{extra1}{a column in \code{df1} which does not exist in \code{df2}}
}
}
}
\usage{
df1
}
\description{
An example data frame for testing compareDFx
}
\keyword{datasets}
