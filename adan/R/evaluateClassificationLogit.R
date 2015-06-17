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
evaluateClassificationLogit<-
  function(adriMdl, newX , newY ,newP ,  dfclass = NULL , print=T){
    if(is.null(dfclass)){
      dfclass <- classifyADRIdf(adriMdl = adriMdl , newX  , newP )$dfclass
    }
          
    absE = abs(newY-newP)  
    aovr <- glm( formula = absE ~ . ,as.data.frame( dfclass),family = "binomial")
    if(print){
      print(summary(aovr))
    }
    return(list(dfclass = dfclass,aovr=aovr))    
}
