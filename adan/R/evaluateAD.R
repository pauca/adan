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
