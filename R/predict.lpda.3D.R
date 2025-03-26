predict.lpda.3D <- function(object, datatest=NULL,...)
{
  # when parafac, predict.lpda is aplied to A matrix
  # when no parafac, predict.pda is aplied to each 3rd matrix, computing here the max frecuency
  
  if(!inherits(object, "lpda.3D"))
    stop("object should be of class 'lpda.3D' ")
  group = as.factor(as.character(object$group))
  compare = combn(levels(group),2)
  
if(object$pfac) {
  if(is.null(datatest)){
    # in mod.lpda data is A matrix
    output = predict(object$MOD$mod.lpda)
  }
  else{
    nfac = ncol(object$MOD$mod.pfac$A)
    pfacFixedBC = parafac(datatest, nfac = nfac, Bfixed = object$MOD$mod.pfac$B, Cfixed = object$MOD$mod.pfac$C,
                          verbose = FALSE)
    A.test = pfacFixedBC$A
    output = predict(object$MOD$mod.lpda, datatest = A.test)
  }
  }
else{
    I = dim(object$data)[3]
    n = dim(object$data)[1]
    if(!is.null(datatest)) {
      n =  dim(datatest)[1]
      for(i in 1:I)
      {
        object$MOD[[i]]$data = datatest[,,i]
      }}
    PRED = sapply(object$MOD, predict)
    EVAL = data.frame(PRED[2,])
    CLASS= NULL
    for(i in 1:I)
    {
      CLASS = cbind(CLASS, PRED[,i][[1]])
    }
    my.class = NULL
    for (i in 1:n)
    {
      xi = factor(CLASS[i,],levels=levels(group))
      classi = names(table(xi)[table(xi)==max(table(xi))])
 if(length(classi)>=2 && length(levels(group))==2){
   sumEVAL = abs(tapply(as.numeric(EVAL[i,]),as.factor(CLASS[i,]),sum))
   classi = names(sumEVAL)[sumEVAL==max(sumEVAL)]
 }
if(length(classi)>=2 && length(levels(group))>2){
  n.comp = ncol(compare)
  sumEVAL=NULL
  for (h in 1:I)
  {  
  j=h+(h-1)*(n.comp-1)
  eval.i=as.numeric(EVAL[i,])
  mat.num = matrix(eval.i[j:(j+n.comp-1)], 2, n.comp, byrow=TRUE)
  mat.num[1,][mat.num[1,]<0]=0
  mat.num[2,][mat.num[2,]>0]=0
  mat.num=abs(mat.num)
  mat.logic = compare==CLASS[i,h]
  sumEVAL[h]=sum((mat.logic*1)*mat.num)
  }
  names(sumEVAL)=CLASS[i,]
  classi = names(sumEVAL)[sumEVAL==max(sumEVAL)]
}
    my.class = c(my.class, classi)
    }
    output = list(my.class, EVAL) 
    names(output) = c("fitted", "eval")
  }
 
  class(output) <- "predict.lpda.3D"
  output
}

#-------------------------------------------------------------
print.predict.lpda.3D <- function(x, ...)
{
  # x is an object of class inheriting from "predict.lpda.3D"
  if(!inherits(x, "predict.lpda.3D"))
    stop("object should be of class 'predict.lpda.3D' ")
  cat("\n")
  names(x$fitted) = rownames(x$eval) #nombres individuos
  if(is.null(names(x$fitted))) names(x$fitted) = c(1:length(x$fitted))
  print(x$fitted)
  cat("\n")
}