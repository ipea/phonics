% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statcan.R
\name{statcan}
\alias{statcan}
\title{Statistics Canada Name Coding}
\usage{
statcan(word, maxCodeLen = 4)
}
\arguments{
\item{word}{string or vector of strings to encode}

\item{maxCodeLen}{maximum length of the resulting encodings, in characters}
}
\value{
the Statistics Canada encoded character vector
}
\description{
The modified Statistics Canada name coding procedure
}
\details{
The variable \code{word} is the name to be encoded.  The variable
\code{maxCodeLen} is the limit on how long the returned name code
should be.  The default is 4.
}
\examples{
statcan("William")
statcan(c("Peter", "Peady"))
statcan("Stevenson", maxCodeLen = 8)

}
\references{
Billy T. Lynch and William L. Arends. "Selection of surname coding
procedure for the SRS record linkage system." United States
Department of Agriculture, Sample Survey Research Branch, Research
Division, Washington, 1977.
}
\seealso{
Other phonics: \code{\link{caverphone}},
  \code{\link{cologne}}, \code{\link{lein}},
  \code{\link{metaphone}}, \code{\link{mra_encode}},
  \code{\link{nysiis}}, \code{\link{rogerroot}},
  \code{\link{soundex}}
}

