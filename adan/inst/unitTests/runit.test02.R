
library("RUnit")
library("adan")

 

### --- Test functions ---

test.split <- function()
{
  X <- diag(x = 1, 10, 20 )
  Y <- c(rep(0,5),rep(1,5))
  
  split <- splitTrainTestRandomRegion( X,  Y, perTrain = 35 , seed = 1992 , 
                              scale = F , plot = F ,ncomp = 2)

  checkTrue( nrow(split$Xtrain) + nrow(split$Xtest )==  nrow(X) , ' dim match 1')
  checkTrue( length(split$Ytrain) + length(split$Ytest )==  length(Y) , ' dim match 2')
  checkTrue( nrow(split$Xtrain) == length(split$Ytrain) , 'dim match 3')
  checkTrue( nrow(X ) == length(split$set) , 'dim match 4')
  checkTrue( nrow(split$Xtrain) == 3 , '35 per rule')
}

