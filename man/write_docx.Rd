% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-officer.R
\name{write_docx}
\alias{write_docx}
\alias{write_docx.default}
\alias{write_docx.data.frame}
\alias{write_docx.list}
\alias{write_docx.rdocx}
\title{Write a object to MS-Word}
\usage{
write_docx(x, path = "", ...)

\method{write_docx}{default}(x, path = "", ...)

\method{write_docx}{data.frame}(x, path = "", ...)

\method{write_docx}{list}(x, path = "", ...)

\method{write_docx}{rdocx}(x, path = "", ...)
}
\arguments{
\item{x}{a string, data frame or list.}

\item{path}{file path.}

\item{...}{unused.}
}
\description{
Send objects like data.frame, list, or just simple texts to a MS-Word document
}
