print.adan <-
function( x , ...){
  cat("adri Object:\n")
  cat("Call:\n")
  print(x$call)
  cat("Used dimensions:\n")
  print(x$ncomp)
  cat("Explained X variance:\n")
  print(x$explvar)
  cat("Subojects:\n")
  print(names(x))
}
