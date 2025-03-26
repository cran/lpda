lpdaCV.3D <- function(data, group, scale = FALSE, pfac = FALSE, nfac = 2, nstart = 10, seed=2,
                      CV = "ktest", ntest = 10, R = 10, f1 = NULL, f2 = NULL)
{
  # data is a 3D array
  # ntest is the number of samples in the test-set
  # R is the times the model is evaluated with each PC indicated in PCs vector
  n = nrow(data)
  I = dim(data)[3]
  J = dim(data)[2]
  
  group = as.factor(as.character(group))
  g1 = levels(group)[1]
  g2 = levels(group)[2]
  check = function(x){sum(x==g1)/I}

  Res = NULL
#---------------------------------------------------------------------------------------- 
# original data (no parafac)
#---------------------------------------------------------------------------------------- 
#con 3D no hace falta poner feature, pq solo hay nfac (en lpdaCV hay PC y Variability)
  if(CV=="loo")
  {
    for (i in 1:n){
      Error.j=NULL
      test = i
      group.train = group[-test]
      group.test = group[test]
      data.train = data[-test, ,]
      data.test = array(data[test, ,],dim = c(1,J,I))
  # este segundo bucle es para el caso de varios PCs o varias Variability a chequear  
      for (j in c(1:length(nfac)))
        {
        mod = lpda.3D(data.train, group = group.train, scale = scale, pfac=pfac, nfac=nfac[j], f1 = f1, f2 = f2)
        pred = predict.lpda.3D(mod, datatest = data.test )
        Error.j = c(Error.j, sum(group.test!=pred$fitted) )
        }
      Res = rbind(Res, Error.j)
    }
    colnames(Res) = paste("nfac", as.character(nfac), sep="-")
    if(!pfac){ colnames(Res) = "Original Data"}
  rownames(Res) = paste("Error individual",c(1:n),sep=".")
  Er_M = apply(Res, 2, mean, na.rm=TRUE) # proportion error rate    
  }
#----------------------------------------------------------------------------------------
  else if(CV=="ktest")
  {
  for (i in 1:R){
    Error.j=NULL
    test = sample(1:n, ntest)
    group.train = group[-test]
    group.test = group[test]
    data.train = data[-test, ,]
    data.test = data[test, ,]
    if(ntest==1) data.test = array(data[test, ,],dim = c(1,J,I))
    # este segundo bucle es para el caso de varios PCs o varias Variability a chequear  
    for (j in c(1:length(nfac)))
    {
    mod = lpda.3D(data.train, group = group.train, scale = scale, pfac=pfac, nfac=nfac[j], f1 = f1, f2 = f2)
    pred = predict.lpda.3D(mod, datatest = data.test )
    Error.j = c(Error.j, sum(pred$fitted!=group.test))
    }
    Res = rbind(Res, Error.j)
  }
    colnames(Res) = paste("nfac", as.character(nfac), sep="-")
    if(!pfac){ colnames(Res) = "Original Data"}
    rownames(Res) = paste("Errors iteration",c(1:R),sep=".")
    Er_M = apply(Res, 2, mean, na.rm=TRUE)/ntest # proportion error rate   
  }
  else
  output=paste(CV,"is not a specified Cross-validation method")    
#----------------------------------------------------------------------------------------
  
  output = list(Res, Er_M)
  names(output) <- c("Matrix.Error", "Error.Rate")
  
  output$call = match.call()
  class(output)<-"lpdaCV" #same results than lpdaCV. No need different print
  output
}



