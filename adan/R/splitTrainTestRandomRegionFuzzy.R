splitTrainTestRandomRegionFuzzy <-
function(
  X,  Y, perTrain = 35 , seed = 1992 , scale = F , plot = TRUE , ncomp = 2
){
  if(!is.logical(scale)) error("scale takes only logical values")
  scale= ifelse(scale,"uv", "none")
  
  if(ncomp < 2) ncomp = 2
  set.seed(seed)
  pcaMdl <- pca(X, nPcs=ncomp,scale=scale)
  center <- sample(1:nrow(X),1)
  d <- apply( t(pcaMdl@scores )- pcaMdl@scores[center,], 2,
         function(x)sqrt(crossprod(x)))
  indx <- sample( 1:length(d), size=floor(nrow(X)*(perTrain/100) ), prob=exp(- ( 2*d/sd(d)))^2 )
  
  set <- rep("Test",nrow(X))
  set[indx] <- "Train"
  
  g1 <- NULL
  if(plot){
    df <- cbind(as.data.frame(pcaMdl@scores[,1:2]), data.frame(Set=factor(set)))
    g1 <- ggplot(df)+geom_point(aes(PC1,PC2,color=Set),shape=18)+
                    ggtitle("Train Test Split")
    print(g1)
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
