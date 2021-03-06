% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mauchly-test.R
\name{mauchly_test}
\alias{mauchly_test}
\title{Mauchly's test of sphericity}
\usage{
mauchly_test(
  model,
  digits.pvalue = 3,
  language = c("en", "chn"),
  table.number = NULL
)
}
\arguments{
\item{model}{a object from \link{rmanova_long} or \link{rmanova_wide}.}

\item{digits.pvalue}{digits for p value, default 3.}

\item{language}{language, 'en' for English, 'chn' for Chinese.}

\item{table.number}{table number.}
}
\value{
a data.frame.
}
\description{
Mauchly's test of sphericity
}
