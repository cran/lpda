\name{summary.lpda}
\alias{summary.lpda}
\alias{print.summary.lpda}

\title{Summarizing lpda classification}

\description{
\code{summary} method for class "lpda"
}

\usage{
\method{summary}{lpda}(object, datatest = object$data, grouptest=object$group,...)

\method{print}{summary.lpda}(x, ...)
}

\arguments{
\item{object}{Object of class inheriting from "lpda"}

\item{datatest}{An optional data to predict their class and compare with real in the confusion matrix. If omitted, the original data is used.}

\item{grouptest}{When datatest is specified, grouptest must also be specified and viceversa.}

\item{x}{An object of class "\code{summary.lpda}", a result of a call to \code{summary.lpda}.}

\item{...}{Other arguments passed.}
}

\value{
  \item{Confusion.Matrix}{Table of confusion. Predicted classes in rows and real classes in columns, giving the hit (in the diagonal) and misclassification counts (out of the diagonal) }
  }

\author{Maria Jose Nueda, \email{mj.nueda@ua.es}}

\seealso{ \code{\link{lpda}}}

\keyword{classification}
