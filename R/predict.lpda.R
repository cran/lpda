predict.lpda <- function(object, datatest = object$data,...)
{
  # lpda.model must be "lpda" class

  if(!inherits(object, "lpda"))
   stop("object should be of class 'lpda' ")

  group = as.factor(as.character(object$group))
  compare = combn(levels(group),2)

  # when datatest is only one sample
  if(is.null(nrow(datatest))){
    datatest = data.frame(t(datatest))
  }

  # When pca=TRUE X has loadings object. In such case data are proyected.
    if(!is.null(object$loadings))
  {
    # when pca data is always scaled (using data.training)
    x.i = stand2(object$data, datatest)
    pc.i = data.frame(x.i%*%object$loadings)
    datatest=pc.i
  }
  if(is.null(object$loadings)){
  # datatest must be standarised with data mean and sd
  if(object$scale) datatest = stand2(object$data, datatest)}

q = nrow(object$coef)
Eval = NULL
for(i in 1:nrow(datatest))
{
  eval = NULL
  for(j in 1:ncol(object$coef))
  {
  eval = c(eval, sum(object$coef[-q,j]*datatest[i,])-object$coef[q,j])
  }
  Eval = rbind(Eval,eval)
}
rownames(Eval) = rownames(datatest)


Mclass = matrix(NA, nrow(datatest), ncol(object$coef))
for (i in 1:nrow(datatest))
{for(j in 1:ncol(Eval))
{
  comp.j = as.factor(compare[,j])
  if(Eval[i,j]>0) Mclass[i,j] = levels(comp.j)[1]
  else if(Eval[i,j]<0) Mclass[i,j]=levels(comp.j)[2]
}}

my.class = NULL
for (i in 1:nrow(datatest))
{
  xi = factor(Mclass[i,],levels=levels(group))
  classi = names(table(xi)[table(xi)==max(table(xi))])
  if(length(classi)>=2){
    sumEVAL = abs(tapply(as.numeric(Eval[i,]),as.factor(Mclass[i,]),sum))
    classi = names(sumEVAL)[sumEVAL==max(sumEVAL)]
  }
  my.class = c(my.class, classi)
}

#my.class = apply(Mclass,1,moda)
output = list(my.class, Eval)
names(output) = c("fitted", "eval")
class(output) <- "predict.lpda"
output
}

#-------------------------------------------------------------
# moda <- function(x) {
#   return(names(which.max(table(x))))}
#-------------------------------------------------------------
print.predict.lpda <- function(x,...)
{
  # x is an object of class inheriting from "predict.lpda"
  if(!inherits(x, "predict.lpda"))
    stop("object should be of class 'predict.lpda' ")
  cat("\n")
  names(x$fitted) = rownames(x$eval)
  if(is.null(names(x$fitted))) names(x$fitted) = c(1:length(x$fitted))
  print(x$fitted)
  cat("\n")
}