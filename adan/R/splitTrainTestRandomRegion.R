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
splitTrainTestRandomRegion <-
function(
  X,  Y, perTrain = 35 , seed = 1992 , scale = F , plot = TRUE , ncomp = 2
){
  if(!is.logical(scale)) error("scale takes only logical values")
  scale= ifelse(scale,"uv", "none")
  
  if(ncomp < 2) ncomp = 2
  set.seed(seed)
  pcaMdl <- pca(X, nPcs=ncomp,scale=scale )
  center <- sample(1:nrow(X),1)
  d <- apply( t(pcaMdl@scores )- pcaMdl@scores[center,], 2,
         function(x)sqrt(crossprod(x)))
  indx <- order( d )[1:floor(nrow(X)*(perTrain/100) )]
  set <- rep("Test",nrow(X))
  set[indx] <- "Train"
  
 
  if(plot){
    df <- cbind(as.data.frame(pcaMdl@scores[,1:2]), data.frame(Set=factor(set)))
    g1 <- ggplot(df)+geom_point(aes(PC1,PC2,color=Set),shape=18)+
                    ggtitle("Train Test Split")
    print(g1)
  }else{
    g1 <- NULL
  }
  list(
    Xtrain = X[set == "Train",],
    Xtest  = X[set == "Test",],
    Ytrain = Y[set == "Train"],
    Ytest  = Y[set == "Test"],
    set    = set,
    center = center  ,  
    plot   = g1,
    call   = match.call(),
    ncomp  = ncomp
    )  
}
