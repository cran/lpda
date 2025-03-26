summary.lpda <- function(object, datatest = object$data, grouptest=object$group,...)
{
  # object is an object of class inheriting from "lpda"
  if(!inherits(object, "lpda"))
    stop("object should be of class 'lpda' ")
#  cat("Call:\n")
  pred = predict(object, datatest = datatest)
  Predicted = pred$fitted # renamed for tablenames
  Real = grouptest        # renamed for tablenames
  TAB = table(Predicted, Real)
  res <- list(call=object$call,
              Confusion.Matrix = TAB)
  class(res) <- "summary.lpda"
  res
}

print.summary.lpda <- function(x, ...)
{
  # x is an object of class inheriting from "summary.lpda"
  if(!inherits(x, "summary.lpda"))
    stop("object should be of class 'summary.lpda' ")
  cat("Call:\n")
  print(x$call)
  cat("\n")
  cat("CONFUSION MATRIX")
  cat("\n")
#  cat("Predicted / Real") 
  print(x$Confusion.Matrix, ...)
  cat("\n")
}



