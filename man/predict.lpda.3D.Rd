\name{predict.lpda.3D}
\alias{predict.lpda.3D}
\alias{print.predict.lpda.3D}

\title{Predict method for lpda.3D classification}

\description{
Predict method for lpda.3D classification
}

\usage{
\method{predict}{lpda.3D}(object, datatest = NULL,...)

\method{print}{predict.lpda.3D}(x, ...)
}

\arguments{
\item{object}{Object of class inheriting from "lpda.3D"}

\item{datatest}{An optional data to predict their class. If omitted, the original data is used.}

\item{x}{An object of class "\code{predict.lpda.3D}", a result of a call to \code{predict.lpda.3D}.}

\item{...}{Other arguments passed.}
}

\value{
  \item{fitted}{Predicted class}
  \item{eval}{Evaluation of each individual in all the fitted models}
  }

\author{Maria Jose Nueda, \email{mj.nueda@ua.es}}

\seealso{ \code{\link{lpda}}}

\keyword{classification}
