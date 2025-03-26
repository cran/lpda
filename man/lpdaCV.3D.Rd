\name{lpdaCV.3D}
\alias{lpdaCV.3D}

\title{Crossvalidation procedure for lpda3D evaluation}

\description{
lpdaCV.3D evaluates the error rate classification with a crossvalidation procedure
}

\usage{

lpdaCV.3D(data, group, scale = FALSE, pfac = FALSE, nfac = 2, nstart = 10, seed=2,
                       CV = "ktest", ntest = 10, R = 10, f1 = NULL, f2 = NULL)
}
\arguments{
\item{data}{Array containing data. Individuals in the first mode, variables in the second mode and third mode with time or similar.}

\item{group}{Vector with the variable group.}

\item{scale}{Logical indicating if it is required standardize data.}

\item{pfac}{Logical indicating if Parafac Analysis is required}

\item{nfac}{Number of factors for Parafac Analysis. By default it is 2.}

\item{nstart}{Number of random starts for multiway analysis.}

\item{seed}{A single value to reproduce same results in multiway methods. If NULL the start will be random.}

\item{CV}{Crossvalidation mode: loo "leave one out" or ktest: that leaves k in the test set.}

\item{ntest}{Number of samples to evaluate in the test-set.}

\item{R}{Number of times that the error is evaluated.}

\item{f1}{Vector with weights for individuals of the first group. If NULL they are equally weighted.}

\item{f2}{Vector with weights for individuals of the second group. If NULL they are equally weighted.}
}

\value{

\code{lpda.3D} returns the prediction error rate classification.

}

\author{Maria Jose Nueda, \email{mj.nueda@ua.es}}

\seealso{ \code{\link{lpda.3D}}, \code{\link{lpdaCV}}}

\examples{

### RNAseq is a 3-dimensional array
  \donttest{
  data(RNAseq)
  group = as.factor(rep(c("G1","G2"), each = 10))
  lpdaCV.3D(RNAseq, group , CV = "ktest", R=5, ntest=5, pfac=TRUE, nfac=c(2,10))
  }
  }

\keyword{classification}
