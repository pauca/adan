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
