lpda.pca <- function(data, group, PC = 2, Variability = NULL)
{
  # This opcion is recomanable when p>>n.
  # By default it is applied lpda.pca with 2 CP
  # When Variability is given, lpda.pca selects the PCs

  group = as.factor(group)
  data.s = stand(data)
  pca = PCA(data.s)

  scores = pca$scores
  loadings = pca$loadings
  colnames(scores) = colnames(loadings)=paste("C",1:ncol(scores),sep="")
  var.exp = pca$var.exp

  #----------------------------------------------------------
  # Choosing the number of PCs: q
  #----------------------------------------------------------
  if(!is.null(Variability)) {q = min(which(var.exp[,2]>Variability)) }
  else q = PC
  #----------------------------------------------------------
  if(q==1) {
 # message("One PC explains ", round(var.exp[1,2],2)*100, "%. The analysis is done with 2 PCs.", sep="")
 # Not necessary this message. Explained in Variability argument.
    q=2
  }
    scores = scores[,1:q]
    loadings = loadings[,1:q]

   res = list(loadings, scores, var.exp, q)
   names(res) = c("loadings", "scores", "var.exp","PCs")
   res
  }


