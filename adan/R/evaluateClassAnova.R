evaluateClassificationAnova <-
  function(adriMdl, newX , newY ,newP ,  dfclass = NULL , print=T){
    if(is.null(dfclass)){
      dfclass <- classifyADRIdf(adriMdl = adriMdl , newX  , newP )$dfclass
    }
          
    absE = abs(newY-newP)  
    aovr <- aov( absE ~ . ,as.data.frame( dfclass))
    if(print){
      print(summary(aovr))
    }
    return(list(dfclass = dfclass,aovr=aovr))    
}
