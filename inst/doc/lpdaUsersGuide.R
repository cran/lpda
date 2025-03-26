## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "figures/README-",
  out.width = "100%", 
  fig.height=5, 
  fig.width=7
)
knitr::opts_chunk$set(fig.pos = "!h", fig.align="center")

## ----setup--------------------------------------------------------------------
library(lpda) 

## -----------------------------------------------------------------------------
data("palmdates")
names(palmdates)
dim(palmdates$spectra)
names(palmdates$conc)

## -----------------------------------------------------------------------------
data("RNAseq")
dim(RNAseq)
head(RNAseq[,1:6,1:2])

## ----echo = TRUE, message = FALSE---------------------------------------------
   group = as.factor( c(rep("Spanish",11), rep("Other",10)) )
   model1 = lpda(data = palmdates$conc[,1:2], group = group )
   model1
   names(model1)

## ----echo = TRUE, message = FALSE---------------------------------------------
   plot(palmdates$conc[,1:2], col = as.numeric(group)+1, pch = 20, 
   main = "Model 1. Palmdates: fibre & sorbitol")
   abline(model1$coef[3]/model1$coef[2], -model1$coef[1]/model1$coef[2], cex = 2)
   legend("bottomright", c("Other","Spanish"),col = c(2,3), pch = 20, cex=0.8)

## -----------------------------------------------------------------------------
predict(model1)
summary(model1)

## ----echo = TRUE, message = FALSE---------------------------------------------
   model2 = lpda(data = palmdates$conc, group = group )
   plot(model2, main="Model 2. Palmdates: all conc variables")

## ----echo = TRUE, message = FALSE---------------------------------------------
model3 = lpda(data = palmdates$conc, group = group, pca = TRUE, Variability = 0.7)
plot(model3, PCscores = TRUE, main = "Model 3. Palmdates: PCA on conc variables")

## ----echo=FALSE---------------------------------------------------------------
X=as.matrix(palmdates$spectra)
col=as.numeric(group)+1
plot(X[1,],type="l",xlab="Raman shift/cm-1",ylab="" , ylim=c(min(X),max(X)),col=col[1],
     main="Palmdates-Spectra")
for(i in 2:21){
  lines(X[i,],col=col[i]) }
legend("topleft", c("Other","Spanish"),col = c(2,3),lty = 1, cex=0.8)


## ----echo = TRUE, message = FALSE---------------------------------------------
model4 = lpda(data = palmdates$spectra, group = group, pca = TRUE, Variability = 0.9)
plot(model4, PCscores = TRUE, main = "Model 4. Palmdates: PCA on Spectra variables")

## ----echo = TRUE, message = FALSE---------------------------------------------
test = c(10,11,12,13)
model5 = lpda(data = palmdates$spectra[-test,], group = group[-test], pca = TRUE,
              Variability = 0.9)
predict(model5,  datatest=palmdates$spectra[test,])
summary(model5, datatest=palmdates$spectra[test,], grouptest = group[test])

## ----fig.height=3, fig.width=9, out.width = "100%"----------------------------
model.iris = lpda(iris[,-5], iris[,5])
summary(model.iris)

## ----fig.height=3, fig.width=9, out.width = "100%",echo=FALSE-----------------
oldpar <- par(mfrow = c(1,3))
plot(model.iris)
par(oldpar)

## -----------------------------------------------------------------------------
model.iris2 = lpda(iris[,-5], iris[,5], pca=TRUE, PC=3)
summary(model.iris2)
plot(model.iris2, PCscores= TRUE)

## ----eval=FALSE---------------------------------------------------------------
# lpdaCV(palmdates$spectra, group, pca = TRUE, CV = "loo")
# lpdaCV(palmdates$spectra, group, pca = TRUE, CV = "ktest", ntest = 5, R = 10)

## ----echo = TRUE, results='hide', message = FALSE-----------------------------
  data(RNAseq) # 3-dimensional array
  data = RNAseq[,,3] # the third data matrix with dimensions 20 x 600
  group = as.factor(rep(c("G1","G2"), each = 10))
  model = lpda(data, group) # model with all the variables
  summary(model)

## ----echo = TRUE, message = FALSE---------------------------------------------
lpdaCV(data, group, pca = FALSE, CV = "ktest", ntest = 5)
lpdaCV(data, group, pca = TRUE, CV = "ktest", ntest = 5)

## ----echo = TRUE, message = FALSE---------------------------------------------
lpdaCV(data, group, pca = TRUE, CV = "ktest", Variability = c(0.1, 0.9))
lpdaCV(data, group, pca = TRUE, CV = "ktest", PC= c(2, 10))

## ----echo = TRUE--------------------------------------------------------------
  data(RNAseq) # 3-dimensional array
  dim(RNAseq)
  group = as.factor(rep(c("G1","G2"), each = 10))

## ----echo = TRUE--------------------------------------------------------------
  model3D = lpda.3D(RNAseq, group)
  summary(model3D)
  predict(model3D)
  plot(model3D, mfrow=c(2,2))

## ----echo = TRUE--------------------------------------------------------------
  model3Ds2 = lpda.3D(RNAseq, group, pfac=TRUE, nfac=2)
  model3Ds2$MOD$mod.pfac$Rsq
  predict(model3Ds2)
  summary(model3Ds2)
  plot(model3Ds2, pfacscores=FALSE, main="Parafac Model")
  plot(model3Ds2, pfacscores=TRUE, cex=1.5, main="Parafac components")

## ----echo = TRUE--------------------------------------------------------------
  lpdaCV.3D(RNAseq, group , CV = "ktest", R=5, ntest=5, pfac=TRUE, nfac=c(2,10))

