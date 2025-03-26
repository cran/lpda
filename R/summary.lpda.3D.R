summary.lpda.3D <- function(object, datatest=NULL, grouptest=NULL,...)
{
  # object is an object of class inheriting from "lpda"
  if(!inherits(object, "lpda.3D"))
    stop("object should be of class 'lpda' ")
  #  cat("Call:\n")
  if( sum(is.null(datatest),is.null(grouptest))==1)  {
    stop("When datatest is specified, grouptest must also be specified and viceversa.")
  }
  if( all(is.null(datatest),is.null(grouptest))) {
    grouptest = object$group
  }
  
  pred = predict(object, datatest=datatest,...) #grouptest no need in predict
  Predicted = pred$fitted # renamed for tablenames
  Real = grouptest        # renamed for tablenames
  TAB = table(Predicted, Real) 
  res <- list(call=object$call,
              Confusion.Matrix = TAB)
  class(res) <- "summary.lpda.3D"
  res
}

#-------------------------------------------------------------
print.summary.lpda.3D <- function(x, ...)
{
  # x is an object of class inheriting from "summary.lpda.3D"
  if(!inherits(x, "summary.lpda.3D"))
    stop("object should be of class 'summary.lpda' ")
  #cat("Call:\n")
  #print(x$call)
  #cat("\n")
  cat("CONFUSION MATRIX")
  cat("\n")
 # cat("Predicted / Real")
  print(x$Confusion.Matrix, ...)
  cat("\n")
}




