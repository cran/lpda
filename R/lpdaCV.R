lpdaCV <- function(data, group, scale = FALSE, pca = FALSE, PC = 2, Variability = NULL,
                   CV = "ktest", ntest = 10, R = 10, f1 = NULL, f2 = NULL)
  {
  group = as.factor(group)
  n = nrow(data)
  
  feature = Variability
  name.feature = "Variability"
  if(is.null(Variability)){
    feature = PC
    name.feature = "PC"}
  
  if(!pca){
   feature = Variability = PC = 1
   name.feature = NULL}
  Res = NULL
#----------------------------------------------------------------------------------------  
  if(CV=="loo")
  {
    for (i in 1:n){
      Error.j=NULL
# este segundo bucle es para el caso de varios PCs o varias Variability a chequear  
      for (j in c(1:length(feature)))
      {
      model = lpda(data[-i,], group[-i], scale = scale, pca = pca, PC=PC[j], 
                   Variability = Variability[j], f1 = f1, f2 = f2)
      pred = predict(model, data[i,])
      Error.j = c(Error.j, sum(pred$fitted!=group[i]))
      }
      Res = rbind(Res, Error.j)
     }
    colnames(Res) = paste(name.feature, as.character(feature), sep="-")
    if(!pca){ colnames(Res) = "Original Data"}
    rownames(Res) = paste("Error individual",c(1:n),sep=".")
    Er_M = apply(Res, 2, mean, na.rm=TRUE) # proportion error rate    
  }

#----------------------------------------------------------------------------------------
  else if(CV=="ktest")
  {
    for (i in 1:R){
      test = sample(1:n, ntest)
      group.train = group[-test]
      group.test = group[test]
      data.train = data[-test, ]
      data.test = data[test, ]

    Error.j=NULL
# este segundo bucle es para el caso de varios PCs o varias Variability a chequear  
    for (j in c(1:length(feature)))
    {
      model = lpda(data = data.train , group = group.train, scale =scale, pca = pca, PC=PC[j], 
                   Variability = Variability[j], f1 = f1, f2 = f2)
      pred = predict(model, data.test)
      Error.j = c(Error.j, sum(pred$fitted!=group.test))
    }
    Res = rbind(Res, Error.j)
  }
   colnames(Res) = paste(name.feature, as.character(feature), sep="-")
   if(!pca){ colnames(Res) = "Original Data"}
   rownames(Res) = paste("Errors iteration",c(1:R),sep=".")
   Er_M = apply(Res, 2, mean, na.rm=TRUE)/ntest # proportion error rate    
  }
#----------------------------------------------------------------------------------------
  else
    output=paste(CV,"is not a specified Cross-validation method")

#----------------------------------------------------------------------------------------  
  
  output = list(Res, Er_M)
  names(output) <- c("Matrix.Error", "Error.Rate")
  
  output$call = match.call()
  class(output)<-"lpdaCV"
  output
}

#------------------------------------------------------------------------------------
print.lpdaCV <- function(x,...)
{
  # x is an object of class inheriting from "lpdaCV"
  if(!inherits(x, "lpdaCV"))
    stop("x should be of class 'lpdaCV' ")
  cat("Call:\n")
  print(x$call)
  
  # cat("Errors of each data test: \n")
  # print(x$Matrix.Error, ...)
  
  cat("Prediction Error Rate for analysed model/models: \n")
  print(x$Error.Rate, ...)
  cat("\n")
}
