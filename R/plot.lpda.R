plot.lpda<-function(x, PCscores = FALSE, main = NULL, xlab = NULL, ylab = NULL,
                    col= NULL, pch = NULL, lty = NULL,
                    legend.pos = "topright",...)
{
  # x is an object of class inheriting from "lpda"
  if(!inherits(x, "lpda"))
    stop("x should be of class 'lpda' ")
  if(is.null(pch)) pch=20
  if(is.null(lty)) lty=2
  class = as.numeric(x$group)  
  ngroups = length(unique(x$group))
  if(is.null(col)){
    cols.f = factor(x$group, labels=c(2:(1+ngroups)))
    cols = as.numeric( as.character(cols.f) )
    col = as.numeric(levels(cols.f))}
  else{
    cols = as.numeric(as.character(factor(class, labels=col)))
    cols.f = factor(cols)}
  
  if(PCscores){
    if(!x$pca)  stop("PCscores only can be plotted when 'pca=TRUE' in 'lpda'")
    vars = round(100*x$var.exp[,1],2)
    if(is.null(xlab)) xlab = paste("PC-1 ",vars[1],"%",sep="")
    if(is.null(ylab)) ylab = paste("PC-2 ", " ",vars[2],"%",sep="")
   
    
    plot(x$scores[,c(1,2)], col = cols, pch=pch, main = main,
         xlab=xlab, ylab=ylab, ...)

    if(length(levels(x$group))==2)
    abline(a = x$coef[3]/x$coef[2], b=-x$coef[1]/x$coef[2], cex=2, lty=lty,...)

    if(!is.null(legend.pos))  {
    legend(legend.pos, levels(x$group), col = col, pch=pch) }
  }
 else{
   pred = predict(x, x$data)
   eval = pred$eval
   pares = ncol(eval)

   group = x$group
   compare = combn(levels(group),2)
   group=as.character(group)

   if(is.null(main)){
   for (i in 1:pares){
     main[i]=colnames(eval)[i]
     }}
   
   for (i in 1:pares){
     cases = group%in%compare[,i]
     group.i = group[cases]
     group.i = as.factor(group.i)
     data.i = x$data[cases,]
     class.i = as.numeric(as.factor(group.i))
     n1 = sum(class.i==1)
     n2 = sum(class.i==2)
     n = n1 + n2

     xlim=c(min(eval[cases,i]),max(eval[cases,i]))
     ylim = c(1, max(n1, n2)*1.3)
     eje.y = NULL
     eje.y[class.i==1] = c(1:n1)
     eje.y[class.i==2] = c(1:n2)
     
       cols.i = cols[cases]
       col.i =  levels(cols.f)[levels(as.factor(group))%in%compare[,i]]
 #    else{cols = as.numeric(as.character(factor(class.i, labels=col)))}
     if(is.null(xlab))  xlab="Distance to the hyperplane"
     if(is.null(ylab))  ylab="Index"
     plot(eval[cases,i], eje.y, xlim=xlim, ylim=ylim, col=cols.i, pch=pch,
      xlab=xlab, ylab=ylab, main=main[i],...)
     abline(v=0, lty=lty,...)

     legend(legend.pos, levels(group.i), col=col.i, pch = pch)
} 

 }
  }
