classifyADANdf <-
function(adanMdl, newX , newP , getraw = F){
  
  if(!( is.matrix(newX) | is.data.frame(newX))){ stop("X must be a matrix or data.frame ")}
  if(!is.vector(newP)){ stop("P must be a vector.")}
  if(nrow(newX)!=length(newP)){ stop("Dimensions of newX and newP do not match.")}
  if(ncol(newX)!=ncol(adanMdl$XTrain)){ stop("Dimensions of newX and stored Train X do not match.")}
  
  ret <- list()
  if(adanMdl$scale){
    indx <- (adanMdl$scaleSd > 0.01)
    newX[,indx] <- apply(newX[,indx],1,function(x) (x-adanMdl$scaleMean[indx])/ adanMdl$scaleSd [indx])
  }
  newXs  <- predict( adanMdl$plsMdl , newX,type = c("scores") )
  newXp  <- as.matrix(newXs[,1:adanMdl$ncomp])
  newXpC <- as.matrix(newXs[,-c(1:adanMdl$ncomp)])
  
  # distance to X centroid 
  d2cXTe <- apply(newXp,1, function(x) sqrt(crossprod(x-adanMdl$centroid))) 
  ret$d2cXTe <- ifelse( d2cXTe <= adanMdl$d2cXTh, 0 , 1 ) 
  #
  # distance to X neighbor
  d2nXTe <- apply(newXp,1, function(x){
    min(apply(t(adanMdl$Xp)-x ,2 ,function(y)sqrt(crossprod(y))))
  }) 
  ret$d2nXTe <- ifelse( d2nXTe <= adanMdl$d2nXTh, 0 , 1 )
  #
  # distance to model
  #d2mTe <- apply(newXpC,1, function(x) sqrt(crossprod(x))) 
  plsMdl <- adanMdl$plsMdl

  d <- apply( ( t( newX ) - plsMdl$Xmeans) , 2, function(x) sqrt( sum(x*x) ))
  s <- apply( newXp %*% t(plsMdl$loadings[,1:adanMdl$ncomp]) , 1, 
              function(x) sqrt( sum(x*x) ))

  d2mTe <- sqrt( abs( d*d - s*s )/ (ncol(adanMdl$XTrain)- adanMdl$ncomp ))  
  ret$d2mTe <- ifelse( d2mTe <= adanMdl$d2mTh, 0 , 1 )
  
  if(!adanMdl$qualitative){
     #quantitative
    #
    # distance to Y centroid
    d2cYTe <- abs( newP - adanMdl$d2cYTrMean)
    ret$d2cYTe <- ifelse( d2cYTe <= adanMdl$d2cYTh, 0 , 1 )
    #
    # distance to neighbor Y
    d2nYTe <- abs( newP - apply(newXp,1, function(x){
      adanMdl$YTrain[order(apply(t(adanMdl$Xp)-x ,2 ,function(y)sqrt(sum(y*y))))[1]]
    }))
    #d2nYTe <- min( abs(adanMdl$YTrain - newP ) )
    ret$d2nYTe <- ifelse( d2nYTe <= adanMdl$d2nYTh, 0 , 1 )
    # local SDEP
    PY <-  ( adanMdl$PTrain - adanMdl$YTrain ) 
    n10p <-   round(0.05*length(PY))  
    if(n10p<1){ n10p <- 1 }
    d2nSDEP <-apply(newXp,1, function(x){
      t <- PY[order(apply(t(adanMdl$Xp)-x ,2 , crossprod ))[1:n10p]] 
      sqrt( sum(t^2)/(length(t) ))
    })  
    ret$d2nSDEP <- ifelse( d2nSDEP <= adanMdl$d2nSDEPTh, 0 , 1 )
  #
  }else{
    ret$d2cYTe <- NA
    ret$d2nYTe <- NA
    PY <-   (adanMdl$PTrain - adanMdl$YTrain)
    n10p <- round(0.05*length(PY))  
    d2nSDEP <-apply(newXp,1, function(x){
      t <-  PY[order(apply(t(adanMdl$Xp)-x ,2 ,function(y)sqrt(crossprod(y))))[1:n10p]] 
      (sum(t==0)-sum(t!=0))/length(t)
    })
    # note the inequality
    ret$d2nSDEP <- ifelse( d2nSDEP >= adanMdl$d2nSDEPTh, 0 , 1 )
    
  }
  
  
  ret$dfclass <- as.data.frame(do.call(cbind,ret))
  if(getraw){
  ret$dfraw   <- data.frame( d2cXTe , d2nXTe ,  d2mTe , d2cYTe, d2nYTe,d2nSDEP)	
  }
  ret
}
