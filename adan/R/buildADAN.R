# This file is part of adan.
# 
# adan is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# adan is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with adan.  If not, see <http://www.gnu.org/licenses/>.
buildADAN <-
function(
  X, Y, P  , scale = F , ncomp = NULL , explvar = 80 , threshold = 0.95
){
  # X : a matrix with points set in rows
  # Y : a vector with activities
  # scale: 
  # ncomp: number of LV to be used
  #
  ret$threshold <- threshold
  i95 <- round(nrow(X)*threshold) 
  #print(nrow(X))
  #print(i95)
  if( length(Y) != length(P)) stop("Wrong Input. A dimension issue with P or Y")
  if( nrow(X) != length(P)) stop("Wrong Input. A dimension issue with P or X")
  if( nrow(X) != length(Y)) stop("Wrong Input. A dimension issue with Y or X")
  qualitative <- F
  if(is.factor(Y)){ 
    if(!is.factor(P)){ stop("Mix quantitative qualitative not valid")}
    qualitative <- T
    Y <- as.numeric(levels(Y)[Y])
    P <- as.numeric(levels(P)[P])
  }
  ret <- list()
  # calculate SDEP for later CI definition
  ret$SDEP = sd(   Y - P  )
  #
  ret$qualitative = qualitative
  ret$scale = scale
  if(scale){
    ret$scaleSd   <- apply(X,2,sd)
    ret$scaleMean <- apply(X,2,mean)
    indx <- (ret$scaleSd > 0.01)
    X[,indx] <- apply(X[,indx],1,function(x) (x-ret$scaleMean[indx])/ ret$scaleSd [indx])
  }
  ret$call <- match.call()
  th <- threshold
  maxLV = min(100,min( nrow(X)-1  , ncol(X) ))  
 
  if(!is.null(ncomp)){
    if(maxLV<=ncomp)stop("ncomp has to big value!")
  }
  ret$YTrain = as.vector(Y)
  ret$PTrain = as.vector(P)
  # build PLS model
  plsMdl <- plsr(Y ~ .,data=as.data.frame(X) , 
                         ncomp=maxLV, method="oscorespls",scale=F )
  ret$plsMdl = plsMdl
  # if ncomp not provided get one LV that ensures 
  # 50% of X variance explained
  ev <- explvar(plsMdl)
  if(is.null(ncomp)){
    # get first up jump, check plot(ev)
    # ncomp = which( ev[-length(ev)] -  ev[-1] < 0 )[1]
    ncomp = which(sapply(1:length(ev),
      function(i) sum(ev[1:i]))> explvar )[1]
    if(is.na(ncomp)) ncomp = maxLV
  }
  if(!is.numeric(ncomp)) stop("Invalid Dimension!")
  ret$ncomp <- ncomp
  ret$explvar <- sum(ev[1:ncomp])
  # get X projected and complement
 
  Xp  <- as.matrix(as.matrix(plsMdl$scores)[,   1:ncomp ])
  XpC <- as.matrix(as.matrix(plsMdl$scores)[,-c(1:ncomp)])
  ret$XTrain = X
  ret$Xp     = Xp
  ret$XpC    = XpC
  # build distances of first level and thresholds
  #
  # distance to X centroid 
  ret$centroid <- colMeans(Xp)  
  d2cXTr <- apply(Xp,1, function(x) sqrt(crossprod(x-ret$centroid))) 
  #ret$d2cXTh <- quantile(d2cXTr, th , na.rm = T)
 
  ret$d2cXTh <- sort(d2cXTr)[i95]
  d2cXTrb <- ifelse( d2cXTr <= ret$d2cXTh, 0 , 1 )
  #
  # distance to X neighbor
  d2nXTr <- apply(Xp,1, function(x){
   sort(apply(t(Xp)-x ,2 ,function(y)sqrt(crossprod(y))))[2]
  }) 
  #ret$d2nXTh <- quantile(d2nXTr, th  , na.rm = T) 
  ret$d2nXTh <- sort(d2nXTr)[i95]
  d2nXTrb <- ifelse( d2nXTr <= ret$d2nXTh, 0 , 1 )
  #
  # distance to model
  #d2mTr <- apply(XpC,1, function(x) sqrt(crossprod(x))) 
  
  # using pitagoras theorem
  d <- apply( ( t( ret$XTrain ) - plsMdl$Xmeans) , 2, function(x) sqrt( sum(x*x) ))  
  
  s <- apply( Xp %*% t(plsMdl$loadings[,1:ret$ncomp]) , 1, function(x) sqrt( sum(x*x) ))

  d2mTr <- sqrt(  abs( d*d - s*s ) / (ncol(ret$XTrain)- ret$ncomp ))

  
  #ret$d2mTh <- quantile(d2mTr, th  , na.rm = T)
  ret$d2mTh <- sort(d2mTr)[i95]
  d2mTrb <- ifelse( d2mTr <= ret$d2mTh, 0 , 1 )
  #
  # distance to Y centroid
  if(qualitative){
#     nP <- as.numeric(levels(P)[P])
#     nY <- as.numeric(levels(Y)[Y])
    nP <- P
    nY <- Y
    
    d2cYTrb <- NA; d2nYTrb <- NA; 
    d2cYTr <- NA;  d2nYTr <- NA; 
    
    PY <-  ( nP - nY )
    n10p <- max( floor(0.05*length(PY))  )
    d2nSDEP <-apply(Xp,1, function(x){
      t <-  PY[order(apply(t(Xp)-x ,2 ,function(y)sqrt(crossprod(y))))[2:n10p]] 
      (sum(t==0)-sum(t!=0))/length(t)
    })
    ret$d2nSDEPTh <- quantile(d2nSDEP, th  , na.rm = T)
    # note the inequality
    d2nSDEPb <- ifelse( d2nSDEP >= ret$d2nSDEPTh, 0 , 1 )
    
    
  }else{

    #quantitative
    d2cYTr <- abs( Y - mean(Y))
    ret$d2cYTrMean <- mean(Y) 
    #ret$d2cYTh <- quantile(d2cYTr, th , na.rm = T)
    ret$d2cYTh <- sort(d2cYTr)[i95]
    d2cYTrb <- ifelse( d2cYTr <= ret$d2cYTh, 0 , 1 )
#
# distance to neighbor Y
    closeY <- apply(ret$Xp,1, function(x){
      d <- order(apply(t(Xp)-x ,2 ,function(y)sqrt(sum(y*y))))[2]
      ret$YTrain[d]
    })
    d2nYTr <- abs( ret$YTrain - drop(closeY ))
    
#     dclosx <- rep( 1e20, nrow(Xp))
#     dclosy <- rep( 0.0 , nrow(Xp))
#     for( i in 1:nrow(Xp)){
#       closj=0     
#       for( j in 1:nrow(Xp)){
#         if( j != i){
#           t <- drop(Xp[i,])-drop(Xp[j,])
#           t <- sqrt( sum(t*t))
#           if( t < dclosx[i]){
#             dclosx[i] <- t
#             closj <- j            
#           }          
#         }
#       }
#       dclosy[i] <- abs( ret$YTrain[i] - ret$YTrain[closj])
#     }
#     d2nYTr <- dclosy
#     
#     print(sort(dclosx)[i95])
#     print(sort(dclosy))
#     print(sort(dclosy)[i95])
    
    #ret$d2nYTh <- quantile(d2nYTr, th  , na.rm = T )
    #print(sort(d2nYTr))
    ret$d2nYTh <- sort(d2nYTr)[i95]
    d2nYTrb <- ifelse( d2nYTr <= ret$d2nYTh, 0 , 1 )
  
    #
    # SDEP of the neighbourhood
    PY <-  abs( P - Y ) 
    n10p <-  as.integer(round(0.05*nrow(Xp) ))+1
    if(n10p<1){ n10p <- 2 }
    d2nSDEP <-apply(Xp,1, function(x){
      t <- PY[order(apply(t(Xp)-x ,2 , function(y) sum(y*y) ))[2:n10p]]
      sqrt(sum(t^2)/(n10p-1))
    })
    
#     n10p <-  as.integer(floor(nrow(Xp)*0.05))  
#     print(n10p)
#     closerErr <- rep(NA  , n10p )
#     d2nSDEP   <- rep(NA , nrow(Xp))
#     squareErr <- ((ret$PTrain-ret$YTrain)^2)
#     for( i in 1:nrow(Xp)){
#       closerDis <- rep(1e20, n10p )
#       for( j in 1:nrow(Xp)){
#         if( i!=j){
#           t <- Xp[i,]-Xp[j,]
#           t <- sum(t*t)
#           if( t < max( closerDis)){
#             indx <- which(closerDis==max(closerDis)) [1]
#             closerDis[indx] <- t
#             closerErr[indx] <- squareErr[j]
#           }
#         }
#       }
#       d2nSDEP[i] <- sqrt( sum( closerErr )/(n10p))
#     }
     
    
    #ret$d2nSDEPTh <- quantile(d2nSDEP, th  , na.rm = T)
    #print(sort(d2nSDEP))
    ret$d2nSDEPTh <- sort(d2nSDEP)[i95]
    d2nSDEPb <- ifelse( d2nSDEP <= ret$d2nSDEPTh, 0 , 1 )
  }
  
  ret$d2Trb <- data.frame(d2cXTrb, d2nXTrb , d2mTrb ,  d2cYTrb , d2nYTrb , d2nSDEPb)
  ret$d2Tr  <- data.frame(d2cXTr , d2nXTr , d2mTr ,  d2cYTr , d2nYTr , d2nSDEP)
  
  class(ret) <- "adan"
  return(ret)
}
