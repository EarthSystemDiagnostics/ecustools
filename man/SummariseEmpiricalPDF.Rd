% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statistical-functions.R
\name{SummariseEmpiricalPDF}
\alias{SummariseEmpiricalPDF}
\title{Summarise an Empirical Probability Distribution Function.}
\usage{
SummariseEmpiricalPDF(x, p)
}
\arguments{
\item{x}{A vector of values of empirical PDF}

\item{p}{A vector of probabilities}
}
\value{
Returns a named vector with the mean, median, mode, and standard
deviation of the empirical PDF
}
\description{
Summarise an Empirical Probability Distribution Function.
}
\details{
Calculation of the mode is naive. For a multimodal distribution only
the highest is returned, in the case of 2 or more modes with exactly the same
 probability, the first is returned.
}
\examples{
df <- data.frame(x = 1:10)
df$p <- dnorm(df$x, 5, 2)
SummariseEmpiricalPDF(df$x, df$p)
}
