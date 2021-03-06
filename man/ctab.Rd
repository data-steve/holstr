% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ctab.R
\name{ctab}
\alias{ctab}
\title{List Table to Matrix Table}
\usage{
ctab(x)
}
\arguments{
\item{x}{list table to matrix table}
}
\description{
List Table to Matrix Table.
}
\examples{
vec <- c("A", "A", "B", "C")
cv  <- sapply(unique(vec), function(x) length(vec[vec==x]))
ctab(cv)
}

