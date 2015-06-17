
library("RUnit")
library("adan")

### --- Test functions ---

test.quantitative <- function()
{
 
  X <- diag(x = 1, 10, 20 )
  Y <- c(rep(0,5),rep(1,5))
  P <- c(rep(0,4),rep(1,6))
  mdl <- buildADAN( X, Y, P  , scale = F , ncomp = NULL , explvar = 50 , threshold = 0.95 )
  checkTrue( mdl$explvar >= 50 ,'Explained Variance')
  checkTrue( is.numeric(mdl$d2cXTh), 'Threshold')
  checkTrue( is.numeric(mdl$d2nXTh), 'Threshold')
  checkTrue( is.numeric(mdl$d2mTh), 'Threshold')
  checkTrue( is.numeric(mdl$d2cYTh), 'Threshold')
  checkTrue( is.numeric(mdl$d2nYTh), 'Threshold')
  checkTrue( is.numeric(mdl$d2nSDEPTh), 'Threshold')
  checkTrue( round(mdl$SDEP ,7)== 0.3162278 , 'SDEP of original model calculated')
  
  checkTrue(ncol(mdl$XpC)+ncol(mdl$Xp) == (nrow(X)-1) , ' X Matrix Rows')
  checkTrue( mdl$qualitative == F , 'When Quantitative is quantitative')
  checkTrue( any(!is.na(mdl$d2Tr)), 'Train data ok')
  
  checkTrue( length(adan.show(mdl))==2 , 'show two plots' )
}

test.qualitative <- function()
{
  
  X <- diag(x = 1, 10, 20 )
  Y <- factor(c(rep(0,5),rep(1,5)))
  P <- factor(c(rep(0,4),rep(1,6)))
  mdl <- buildADAN( X, Y, P  , scale = F , ncomp = NULL , explvar = 50 , threshold = 0.95 )
  checkTrue( mdl$explvar >= 50 ,'Explained Variance')
  checkTrue( is.numeric(mdl$d2cXTh), 'Threshold')
  checkTrue( is.numeric(mdl$d2nXTh), 'Threshold')
  checkTrue( is.numeric(mdl$d2mTh), 'Threshold')
  checkTrue( is.null(mdl$d2cYTh), 'Threshold')
  checkTrue( is.null(mdl$d2nYTh), 'Threshold')
  checkTrue( is.numeric(mdl$d2nSDEPTh), 'Threshold')
  
  checkTrue(ncol(mdl$XpC)+ncol(mdl$Xp) == (nrow(X)-1) , ' X Matrix Rows')
  checkTrue( mdl$qualitative == T , 'When Qulitative is qualitative')
  checkTrue( any(!is.na(mdl$d2Tr)), 'Train data ok')
}