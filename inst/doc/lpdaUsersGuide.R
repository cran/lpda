## ---- include = FALSE---------------------------------------------------------
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
head(RNAseq[,1:6])

## ---- echo = TRUE, message = FALSE--------------------------------------------
   group = as.factor( c(rep("Spanish",11), rep("Other",10)) )
   model1 = lpda(data = palmdates$conc[,1:2], group = group )

## -----------------------------------------------------------------------------
model1$coef

## ---- echo = TRUE, message = FALSE--------------------------------------------
   plot(palmdates$conc[,1:2], col = as.numeric(group)+1, pch = 20, 
   main = "Palmdates example")
   abline(model1$coef[3]/model1$coef[2], -model1$coef[1]/model1$coef[2], cex = 2)
   legend("bottomright", c("Other","Spanish"),col = c(2,3), pch = 20, cex=0.8)

## -----------------------------------------------------------------------------
pred = predict(model1)
table(pred$fitted, group)

## ---- echo = TRUE, message = FALSE--------------------------------------------
   model2 = lpda(data = palmdates$conc, group = group )
   plot(model2)

## ---- echo = TRUE, message = FALSE--------------------------------------------
model3 = lpda(data = palmdates$conc, group = group, pca = TRUE, Variability = 0.7)
plot(model3, PCscores = TRUE, main = "PCA-Substances palmdates")

## ----echo=FALSE---------------------------------------------------------------
X=as.matrix(palmdates$spectra)
col=as.numeric(group)+1
plot(X[1,],type="l",xlab="Raman shift/cm-1",ylab="" , ylim=c(min(X),max(X)),col=col[1],
     main="Palmdates-Spectra")
for(i in 2:21){
  lines(X[i,],col=col[i]) }
legend("topleft", c("Other","Spanish"),col = c(2,3),lty = 1, cex=0.8)


## ---- echo = TRUE, message = FALSE--------------------------------------------
model4 <- lpda(data = palmdates$spectra, group = group)
pred = predict(model4)
table(pred$fitted, group)

## ---- echo = TRUE, message = FALSE--------------------------------------------
model5 = lpda(data = palmdates$spectra, group = group, pca = TRUE, Variability = 0.9)
plot(model5, PCscores = TRUE, main = "Spectra palmdates")

## ---- echo = TRUE, message = FALSE--------------------------------------------
test = c(10,11,12,13)
model6 = lpda(data = palmdates$spectra[-test,], group = group[-test], pca = TRUE,
              Variability = 0.9)
pred = predict(model6, palmdates$spectra[test,])
pred$fitted

## ----eval=FALSE---------------------------------------------------------------
#  lpdaCV(palmdates$spectra, group, pca = TRUE, CV = "loo")
#  lpdaCV(palmdates$spectra, group, pca = TRUE, CV = "ktest", ntest = 5, R = 10)

## ---- echo = TRUE, message = FALSE--------------------------------------------
  group = as.factor(rep(c("G1","G2"), each = 30))
  model = lpda(RNAseq, group) # model with all the variables
  pred = predict(model)
  table(pred$fitted, group)

## ---- echo = TRUE, message = FALSE--------------------------------------------
lpdaCV(RNAseq, group, pca = FALSE, CV = "ktest", ntest = 10)
lpdaCV(RNAseq, group, pca = TRUE, CV = "ktest", ntest = 10)

## ---- echo = TRUE, message = FALSE--------------------------------------------
bestVariability(RNAseq, group, ntest = 10, R = 10, Vars = c(0.1, 0.9))
bestPC(RNAseq, group, ntest = 10, R = 10, PCs = c(2, 10))

## ---- echo = TRUE, eval=FALSE-------------------------------------------------
#  lpda(RNAseq, group, pca = TRUE, Variability = 0.9)

## ---- fig.height=3, fig.width=9, out.width = "100%"---------------------------
model.iris = lpda(iris[,-5], iris[,5])
pred.iris = predict(model.iris)
table(pred.iris$fitted, iris[,5])

## ---- fig.height=3, fig.width=9, out.width = "100%",echo=FALSE----------------
oldpar <- par(mfrow = c(1,3))
plot(model.iris)
par(oldpar)

## -----------------------------------------------------------------------------
model.iris2 = lpda(iris[,-5], iris[,5], pca=TRUE, PC=3)
pred.iris2 = predict(model.iris2)
table(pred.iris2$fitted, iris[,5])
plot(model.iris2, PCscores= TRUE)

