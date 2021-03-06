% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract-aliases.R
\name{extract-aliases}
\alias{extract-aliases}
\alias{le}
\alias{ve}
\title{Aliases for \code{`[[`} and \code{`[`}}
\usage{
le(x, index)

ve(x, index)
}
\arguments{
\item{x}{List or vector}

\item{index}{Index for list or vector}
}
\description{
\code{le}---as in list element---is an alias for \code{`[[`}, and \code{ve}---as in vector element---is an alias for \code{`[`}.  The functions simply make pipe chains look cleaner.
}
\examples{
iris \%>\% le(1)
iris \%>\% le("Sepal.Width")
iris \%>\%
  le("Sepal.Length") \%>\% 
  ve(1:50) \%>\% 
  wilcox.test \%>\% 
  le("p.value") \%>\% 
  formatp 
}
