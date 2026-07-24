\name{ziggrnorm}
\alias{ziggrnorm}
\alias{ziggsetseed}
\title{Ziggurat Pseudo Random Number Generators Client Package (C++ Header Version)}
\usage{
ziggrnorm(n)

ziggsetseed(s)
}
\arguments{
\item{n}{The size of the returned vector}

\item{s}{The chosen seed}
}
\value{
A vector of pseudo random-number generated draws from the Normal distribution.
}
\description{
Sample client package using \pkg{zigg}, in this version via direct use of
object in the \pkg{zigg} package.
}
\examples{
ziggsetseed(123)
ziggrnorm(3)
}
\references{
<doi:10.18637/jss.v005.i08>
<doi:10.18637/jss.v012.i07>
}
