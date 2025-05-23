\name{lpda}
\alias{lpda}
\alias{print.lpda}

\title{Computing discriminating hyperplane for two groups}

\description{
This function computes a discriminating hyperplane for two groups with original data (calling lpda.fit) or with principal components (calling lpda.pca)
}

\usage{
lpda(data, group, scale = FALSE, pca = FALSE, PC = 2, Variability = NULL,
                 f1 = NULL, f2 = NULL)
\method{print}{lpda}(x, ...)
}
\arguments{
\item{data}{Matrix containing data. Individuals in rows and variables in columns}

\item{group}{Vector with the variable group}

\item{scale}{Logical indicating if it is required standardize data. When pca=TRUE data is always scaled.}

\item{pca}{Logical indicating if Principal Components Analysis is required}

\item{PC}{Number of Principal Components (PC) for PCA. By default it is 2. When the number of PC is not decided, it can be determined choosing the desired proportion of explained variability (Variability parameter).}

\item{Variability}{Parameter for Principal Components (PC) selection. This is the minimum desired proportion of variability explained for the PC of the variables. The analysis is always done with a minimum of 2 PCs. If it is NULL the PCA will be computed with PC parameter.}

\item{f1}{Vector with weights for individuals of the first group. If NULL they are equally weighted.}

\item{f2}{Vector with weights for individuals of the second group. If NULL they are equally weighted.}

\item{x}{An object of class "\code{lpda}", a result of a call to \code{lpda}.}

\item{...}{Other arguments passed.}
}

\value{

\code{lpda} returns an object of class "\code{lpda}".


  \item{coef}{Hyperplane coefficients}
  \item{data}{Input data matrix}
  \item{group}{Input group vector}
  \item{scale}{Input scale argument}
  \item{pca}{Input pca argument}
  \item{loadings}{Principal Components loadings. Showed when pca = TRUE}
  \item{scores}{Principal Components scores. Showed when pca = TRUE}
  \item{var.exp}{A matrix containing the explained variance for each component and the cumulative variance. Showed when pca = TRUE}
  \item{PCs}{Number of Principal Components in the analysis. Showed when pca = TRUE}


The functions \code{predict} and \code{plot} can be used to obtain the predicted classes and a plot in two dimensions with the distances to the computed hyperplane for the two classes.

  }

\references{
Nueda MJ, Gandía C, Molina MD (2022) LPDA: A new classification method based on linear programming. PLoS ONE 17(7): e0270403. <https://doi.org/10.1371/journal.pone.0270403>
}

\author{Maria Jose Nueda, \email{mj.nueda@ua.es}}

\seealso{ \code{\link{lpda.pca}}, \code{\link{lpda.fit}}}

\examples{

######### palmdates example in lpda package:
data(palmdates)
group = as.factor( c(rep("Spanish",11),rep("Foreign",10)) )

# with concentration data:
model = lpda(data = palmdates$conc, group = group )
summary(model)
predict(model)
plot(model, main = "Palmdates example")

model.pca = lpda(data = palmdates$conc, group = group, pca=TRUE, PC = 2)
plot(model.pca, PCscores = TRUE, main = "Palmdates example")

# with spectra data
model.pca = lpda(data = palmdates$spectra, group = group, pca=TRUE, Variability = 0.9)
model.pca$PCs # 4 PCs to explain 90\% of the variability
plot(model.pca, PCscores = TRUE, main = "Spectra palmdates")

}

\keyword{classification}
