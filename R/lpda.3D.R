lpda.3D <- function(data, group, scale = FALSE,  pfac = FALSE, nfac = 2,
                     nstart = 10, seed = 2, f1 = NULL, f2 = NULL)
{
  group = as.factor(as.character(group))
if(pfac) {
  set.seed(seed)
  mod.pfac = parafac(data, nfac = nfac, nstart = nstart, verbose=FALSE)
  A = mod.pfac$A[,1:nfac]
  mod = lpda(A, group, scale, f1=f1, f2=f2)
  MOD = list(mod.pfac, mod)
  names(MOD) = c("mod.pfac", "mod.lpda")
}
 else{ 
  MOD = apply(data, 3, lpda, group, pca=FALSE, scale, f1, f2)
 }
  
  output = list(MOD, data, group, pfac)
  names(output)<-c("MOD", "data", "group", "pfac")
  output$call = match.call()
  class(output)<-"lpda.3D"
  
 output
}

#----------------------------------------------------------------------------
print.lpda.3D <- function(x,...)
{
  # x is an object of class inheriting from "lpda.3D"
  if(!inherits(x, "lpda.3D"))
    stop("x should be of class 'lpda.3D' ")
  cat("Call:\n")
    print(x$call)
    cat("\n")
  if(x$pfac) {
      cat("Coefficients: \n")
      print(as.numeric(x$MOD$mod.lpda$coef), ...)
  }else
      {
    I = dim(x$data)[3]
  for(i in 1:I)
  {
  cat("Coefficients Matrix XX_",i,"\n",sep="")
 # cat(x$MOD[[i]]$coef)
    print(as.numeric(x$MOD[[i]]$coef), ...)
 #  print(as.numeric(x$coef[[i]]), ...)
    }}
  cat("\n")
  cat("\n")
}



