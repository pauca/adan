adan.test <- function( adan.model , query.md , query.p ){
  
  res <- list()
  
  classification <- classifyADANdf (adanMdl = adan.model , newX = query.md , 
                  newP = query.p , getraw = F)$dfclass
  
  res$categories = data.frame(categories = apply( classification  , 1, function(x) sum(x,na.rm=T) ))
  
  # get CI
  res$errorCI <- data.frame( errorCI = adan.model$SDEP * sapply(res$categories[,1] , function(x){
    if(x %in% c(0,1 )) return(1)
    if(x %in% c(2,3 )) return(2)
    return( NA)
  }))
  
  res$classification <- classification
  
  return(res)
}  
  