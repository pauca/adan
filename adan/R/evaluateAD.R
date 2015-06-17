evaluateAD <-
function( adriMdl , newX  , newY , newP , type = "and"){
  Error   <-    ( newY - newP )
  AError  <- abs( newY - newP )
  ADClass <- classifyADRI(adriMdl, newX , newP , type )
  df1 <- data.frame( AError , ADClass ,Error)
  g1 <- ggplot(df1,aes(factor(ADClass),AError,color=Error))+
    geom_boxplot(alpha=0,outlier.size=0)+     
    geom_jitter( alpha=0.6, size=2)+
    ggtitle("Test Set ADRI Classification")+xlab("AD")+ylab("Absolut Error")
  
  list(g1)
}
