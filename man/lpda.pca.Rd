\name{lpda.pca}
\alias{lpda.pca}

\title{lpda.pca computes a PCA to the original data and selects the desired PCs when Variability is supplied}

\description{
\code{lpda.pca} computes the discriminating hyperplane for two groups with Principal Components (PC)
}

\usage{
lpda.pca(data, group, PC = 2, Variability = NULL)
}
\arguments{
\item{data}{Matrix containing data. Individuals in rows and variables in columns}

\item{group}{Vector with the variable group}

\item{PC}{Number of Principal Components (PC) for PCA. By default it is 2. When the number of PC is not decided, it can be determined choosing the desired proportion of explained variability (Variability parameter).}

\item{Variability}{Parameter for Principal Components (PC) selection. This is the minimum desired proportion of variability explained for the PC of the variables. The analysis is always done with a minimum of 2 PCs. If it is NULL the PCA will be computed with PC parameter.}
}

\value{
  \item{loadings}{Principal Components loadings.}
  \item{scores}{Principal Components scores.}
  \item{var.exp}{A matrix containing the explained variance for each component and the cumulative variance.}
  \item{PCs}{Number of Principal Components in the analysis.}
  }

\references{
Nueda MJ, Gandía C, Molina MD (2022) LPDA: A new classification method based on linear programming. PLoS ONE 17(7): e0270403. <https://doi.org/10.1371/journal.pone.0270403>
}

\author{Maria Jose Nueda, \email{mj.nueda@ua.es}}

\seealso{ \code{\link{lpda}}}

\keyword{classification}
