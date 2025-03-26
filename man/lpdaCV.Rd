\name{lpdaCV}
\alias{lpdaCV}
\alias{print.lpdaCV}

\title{Crossvalidation procedure for lpda evaluation}

\description{
lpdaCV evaluates the error rate classification with a crossvalidation procedure
}

\usage{
lpdaCV(data, group, scale = FALSE, pca = FALSE, PC = 2, Variability = NULL,
                   CV = "ktest", ntest = 10, R = 10, f1 = NULL, f2 = NULL)
                   
\method{print}{lpdaCV}(x, ...)
}

\arguments{
\item{data}{Matrix containing data. Individuals in rows and variables in columns}

\item{group}{Vector with the variable group}

\item{scale}{Logical indicating if it is required standardize data.}

\item{pca}{Logical indicating if a reduction of dimension is required}

\item{PC}{Number of Principal Components (PC) for PCA. By default it is 2. When the number of PC is not decided, it can be determined choosing the desired proportion of explained variability (Variability parameter) or choosing the maximum number of errors allowed in the training set (Error.max).}

\item{Variability}{Parameter for Principal Components (PC) selection. This is the desired
    proportion of variability explained for the PC of the variables.}

\item{CV}{Crossvalidation mode: loo "leave one out" or ktest: that leaves k in the test set.}

\item{ntest}{Number of samples to evaluate in the test-set.}

\item{R}{Number of times that the error is evaluated.}

\item{f1}{Vector with weights for individuals of the first group. If NULL they are equally weighted.}

\item{f2}{Vector with weights for individuals of the second group. If NULL they are equally weighted.}

\item{x}{An object of class "\code{lpdaCV}", a result of a call to \code{lpdaCV} or \code{lpdaCV.3D}.}

\item{...}{Other arguments passed.}
}

\value{

\code{lpdaCV} returns the prediction error rate classification.
  }

\author{Maria Jose Nueda, \email{mj.nueda@ua.es}}

\examples{

### RNAseq is a 3-dimensional array
  data(RNAseq)
  data = RNAseq[,,3]
  group = as.factor(rep(c("G1","G2"), each = 10))
  lpdaCV(data, group, pca = TRUE, CV = "ktest", ntest = 2)

  }

\seealso{ \code{\link{lpda}}}

\keyword{classification}
