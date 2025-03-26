plot.lpda.3D<-function(x, pfacscores=FALSE, xlim = NULL, main = NULL, legend.pos = "topright", ...)
{
  # x is an object of class inheriting from "lpda.3D"
  if(!inherits(x, "lpda.3D"))
    stop("x should be of class 'lpda.3D' ")
  if(!pfacscores)
  {
  if(x$pfac){
    par(...)
    plot(x$MOD$mod.lpda, main = main,...)
  }
  else{
    par(...)
    for (i in 1:length(x$MOD)){
    plot(x$MOD[[i]], main = main,...)
    }
  }}
  if(pfacscores)
  {
    if(!x$pfac)  stop("projection only can be plotted when 'pfac=TRUE' in 'lpda.3D'")
    Fac_1 = x$MOD$mod.pfac$A[,1]
    Fac_2 = x$MOD$mod.pfac$A[,2]
    par(...)
   plot(Fac_1, Fac_2, col = as.numeric(x$group)+1, pch=20, main = main, ...)
  }
  }
    