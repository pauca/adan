adan.show <-
function(adanMdl){
    
  AError <- abs( adanMdl$YTrain - adanMdl$PTrain )
  Error <- ( adanMdl$YTrain - adanMdl$PTrain )
  ADClass <- rowSums(adanMdl$d2Tr)
  df1 <- data.frame( AError , ADClass , Error)
  g1 <- ggplot(df1,aes(factor(ADClass),AError,color=Error))+
    geom_boxplot(alpha=0,outlier.size=0)+     
    geom_jitter( alpha=0.6,  size=2)+
    ggtitle("Train Set ADRI Classification - AND")+xlab("AD")+ylab("Error")
  ADClass <- ifelse( rowSums(adanMdl$d2Tr) == 0 , 0 , 1 )
  df2 <- data.frame( AError , ADClass ,Error)
  g2 <- ggplot(df2,aes(factor(ADClass),AError,color=Error))+
    geom_boxplot(alpha=0,outlier.size=0)+     
    geom_jitter( alpha=0.6, size=2)+
    ggtitle("Train Set ADRI Classification - ANY")+xlab("AD")+ylab("Error")
  
  list(g1,g2)
}
