\name{plot.lpda.3D}
\alias{plot.lpda.3D}

\title{Plot method for lpda classification}

\description{
\code{plot.lpda.3D} is applied to an \code{lpda.3D} class object. It shows a plot
in two dimensions with the distances to the computed hyperplane of each individual coloring each case with the real class.
}

\usage{
\method{plot}{lpda.3D}(x, pfacscores = FALSE, main = NULL,
legend.pos = "topright", ...)
}

\arguments{
\item{x}{Object of class inheriting from "lpda"}

\item{pfacscores}{Logical to show the first 2 parafac scores. Only possible when parafac is applied.}

\item{main}{An optional title for the plot.}

\item{legend.pos}{The position for the legend. By default it is topright. NULL when no legend is required.}

\item{...}{Other arguments passed.}
}

\value{
  Two dimensional plot representing the distances to the computed hyperplane of each individual colored with the real class.
  }

\author{Maria Jose Nueda, \email{mj.nueda@ua.es}}


\seealso{ \code{\link{lpda.3D}}, \code{\link{plot.lpda}}}

\keyword{classification}
