\name{summary.lpda.3D}
\alias{summary.lpda.3D}
\alias{print.summary.lpda.3D}

\title{Summarizing lpda.3D classification}

\description{
summary method for class "lpda.3D"
}

\usage{
\method{summary}{lpda.3D}(object, datatest = NULL, grouptest=NULL,...)

\method{print}{summary.lpda.3D}(x, ...)
}

\arguments{
\item{object}{Object of class inheriting from "lpda.3D"}

\item{datatest}{An optional data to predict their class and compare with real in the confusion matrix. If omitted, the original data is used.}

\item{grouptest}{When datatest is specified, grouptest must also be specified and viceversa.}

\item{x}{An object of class "\code{summary.lpda.3D}", a result of a call to \code{summary.lpda.3D}.}

\item{...}{Other arguments passed.}
}

\value{
  \item{Confusion.Matrix}{Table of confusion. Predicted classes in rows and real classes in columns, giving the hit (in the diagonal) and misclassification counts (out of the diagonal) }
  }

\author{Maria Jose Nueda, \email{mj.nueda@ua.es}}

\seealso{ \code{\link{lpda.3D}}}

\keyword{classification}
