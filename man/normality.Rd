% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normality.R
\name{normality}
\alias{normality}
\title{A metric of normality}
\usage{
normality(x)
}
\arguments{
\item{x}{A numeric vector.}
}
\value{
A single value on the scale -1 to 1.
}
\description{
Provides a measure of normality for the input by computing the correlation
between the data and the expected data given each data point's quantile and a
normal distribution with a mean equal to the data's mean and a standard
deviation equal to the data's standard deviation.
}
