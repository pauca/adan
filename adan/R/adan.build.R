adan.build <- function( train.md , train.a , train.p , 
                        scale.md = F){
  buildADAN (
      X= train.md , Y= train.a , P=train.p  , 
      scale = scale.md , ncomp = NULL , 
      explvar = 80 , threshold = 0.95
    )
}