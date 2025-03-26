\name{predict.lpda}
\alias{predict.lpda}
\alias{print.predict.lpda}

\title{Predict method for lpda classification}

\description{
Predict method for lpda classification
}

\usage{
\method{predict}{lpda}(object, datatest = object$data,...)

\method{print}{predict.lpda}(x, ...)
}

\arguments{
\item{object}{Object of class inheriting from "lpda"}

\item{datatest}{An optional data to predict their class. If omitted, the original data is used.}

\item{x}{An object of class "\code{predict.lpda}", a result of a call to \code{predict.lpda}.}

\item{...}{Other arguments passed.}

}

\value{
  \item{fitted}{Predicted class}
  \item{eval}{Evaluation of each individual in the fitted model}
  }

\author{Maria Jose Nueda, \email{mj.nueda@ua.es}}

\seealso{ \code{\link{lpda}}}

\keyword{classification}
