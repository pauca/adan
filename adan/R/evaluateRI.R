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
evaluateRI <-
function( adriMdl , newX  , newY , newP , type = "and"){
  # get ADRI classification
  cl <- classifyADRI(adriMdl, newX , newP , type )
  
  sdep <- sd( adriMdl$PTrain - adriMdl$YTrain )*1.96
  idcl <- sort(unique(cl))
  inIC <- t(sapply( idcl, function(x){
    cond <- (cl == x)
    t <- ifelse( (newP[cond] - sdep) < newY[cond] &  
          newY[cond] < (newP[cond] + sdep)  , TRUE, FALSE)
    c( sum(t) , sum(!t))
  }))
  # build plots
  colnames(inIC) <- c("In","Out")
  df <- as.data.frame(cbind(idcl,inIC))
  mdf <- melt(df,id=c("idcl"))
  mdf$Type = factor(mdf$variable)
  g1 <- ggplot(mdf)+geom_histogram(aes( factor(idcl),value,
                             fill=Type,group=Type) ,
                             stat="identity",position="dodge")+
    ggtitle("")+xlab("AD")+ylab("Inside Confidende Interval")
  
  g2 <- ggplot(mdf)+geom_histogram(aes( factor(idcl),value,
         fill=Type,group=Type) ,
         stat="identity",position="fill")+
         ggtitle("")+xlab("AD")+ylab("Inside Confidende Interval")
  
  list(g1=g1,g2=g2,dfInOut=df)
  #list(g1=g1)
}
