% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/efa_functions.R
\name{tag_efa}
\alias{tag_efa}
\title{Exploratory Factor Analysis (EFA) for Tag Correlations}
\usage{
tag_efa(cor_matrix, n_factors = 4, output = "table")
}
\arguments{
\item{cor_matrix}{A correlation matrix (from \code{correlate_tags()})}

\item{n_factors}{Number of factors to extract. Default is 4.}

\item{output}{Output format. Options:
\itemize{
\item \code{"table"} (default) – Displays an interactive table using \code{DT::datatable}.
\item \code{"dataframe"} – Returns the factor loadings as a data frame.
}}
}
\value{
A table or data frame of factor loadings, with labels applied.
}
\description{
This function conducts an exploratory factor analysis (EFA) based on a correlation
matrix from the \code{correlate_tags()} function. The function returns a table of factor
loadings for the specified number of factors.
}
\details{
The function assumes that the correlation matrix reflects shared variance between
items. Results are presented as either an interactive table (via \code{DT}) or a data frame.
}
\examples{
cor_matrix <- correlate_tags(test_data, output = "dataframe")
tag_efa(cor_matrix, n_factors = 4)

# Return as data frame
factor_results <- tag_efa(cor_matrix, n_factors = 4, output = "dataframe")

}
\seealso{
\code{\link{correlate_tags}} for creating the correlation matrix.
}
