\name{plot.lpda}
\alias{plot.lpda}

\title{Plot method for lpda classification}

\description{
\code{plot.lpda} is applied to an \code{lpda} class object. It shows a plot
in two dimensions with the distances to the computed hyperplane of each individual coloring each case with the real class.
}

\usage{
\method{plot}{lpda}(x, PCscores = FALSE, main = NULL, xlab = NULL, ylab = NULL,
                    col= NULL, pch = NULL, lty = NULL,
                    legend.pos = "topright",...)
}

\arguments{
\item{x}{Object of class inheriting from "lpda"}

\item{PCscores}{Logical to show the first 2 PCscores. Only possible when PCA is applied.}

\item{main}{An optional title for the plot.}

\item{xlab}{An optional title for x-axis.}

\item{ylab}{An optional title for y-axis.}

\item{col}{An optional vector with colours for the groups.}

\item{pch}{An integer specifying the symbol to be used in plotting points. When NULL, pch=20.}

\item{lty}{The line type. If it is not specified, lty = 2 for the distances to the hiperplane and lty = 1 for PCs plot}

\item{legend.pos}{The position for the legend. By default it is topright. NULL when no legend is required.}

\item{...}{Other arguments passed.}
}

\value{
  Two dimensional plot representing the distances to the computed hyperplane of each individual colored with the real class.
  }

\author{Maria Jose Nueda, \email{mj.nueda@ua.es}}


\seealso{ \code{\link{lpda}}}

\keyword{classification}
