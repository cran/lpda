\name{lpda.3D}
\alias{lpda.3D}
\alias{print.lpda.3D}

\title{Classification with lpda for 3way array data}

\description{
This function applies lpda methodology to classify individuals in two or more groups with original data (by applying lpda through the third dimension) or by applying lpda to the parafac scores.
}

\usage{
lpda.3D(data, group, scale = FALSE,  pfac = FALSE, nfac = 2,
                     nstart = 10, seed=2, f1 = NULL, f2 = NULL)
\method{print}{lpda.3D}(x, ...)
}
\arguments{
\item{data}{Array containing data. Individuals in the first mode, variables in the second mode and third mode with time or similar.}

\item{group}{Vector with the variable group.}

\item{scale}{Logical indicating if it is required standardize data.}

\item{pfac}{Logical indicating if Parafac Analysis is required}

\item{nfac}{Number of factors for Parafac Analysis.}

\item{nstart}{Number of random starts for multiway analysis.}

\item{seed}{A single value to reproduce same results in multiway methods.}

\item{f1}{Vector with weights for individuals of the first group. If NULL they are equally weighted.}

\item{f2}{Vector with weights for individuals of the second group. If NULL they are equally weighted.}

\item{x}{An object of class "\code{lpda.3D}", a result of a call to \code{lpda.3D}.}

\item{...}{Other arguments passed.}
}

\value{

\code{lpda.3D} returns an object of class "\code{lpda.3D}".

  \item{MOD}{When \code{pfac=FALSE}, MOD is a list of \code{lpda} objects obtained from the application of lpda to each slide matrix of the third node. When \code{pfac=TRUE}, MOD is a list of two elements: (1) \code{mod.pfac} the parafac model obtained with Multiway package and (2) the \code{lpda} object, obtained from the application of lpda to the parafac scores.}
  \item{data}{Input array data}
  \item{group}{Input group vector}
  \item{pfac}{Input pfac argument}
 
 The functions \code{predict} and \code{plot} can be used to obtain the predicted classes and a plot in two dimensions with the distances to the computed hyperplane for the two classes.
  }

\references{
Nueda MJ, Gandía C, Molina MD (2022) LPDA: A new classification method based on linear programming. PLoS ONE 17(7): e0270403. <https://doi.org/10.1371/journal.pone.0270403>
}

\author{Maria Jose Nueda, \email{mj.nueda@ua.es}}

\seealso{ \code{\link{lpda}}, \code{\link[multiway]{parafac}}}

\examples{

### RNAseq is a 3-dimensional array
  data(RNAseq)
  group = as.factor(rep(c("G1","G2"), each = 10))

## Strategy 1
  model3D = lpda.3D(RNAseq, group)
  summary(model3D)
  predict(model3D)
  plot(model3D, mfrow=c(2,2))

## Strategy 2: with parafac
  model3Ds2 = lpda.3D(RNAseq, group, pfac=TRUE, nfac=2)
  model3Ds2$MOD$mod.pfac$Rsq
  predict(model3Ds2)
  summary(model3Ds2)
  plot(model3Ds2, pfacscores=FALSE, main="Parafac Model", mfrow=c(1,1))
  plot(model3Ds2, pfacscores=TRUE, cex=1.5, main="Parafac components")
  legend("bottomright", levels(group), col=c(2,3), pch=20)
}

\keyword{classification}
