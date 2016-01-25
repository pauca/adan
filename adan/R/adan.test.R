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


#' Perform Prediction Reliability and Applicablity Domain assasemnt
#' 
#' @param adan.model A adan model built with adan.test.
#' @param query.md A matrix with molecular descriptors with query data.
#' @param query.p A vector with predictions for query data.
#' @return A list with applicability domain assesment and confidence interval estimation.
adan.test <- function( adan.model , query.md , query.p , sdep = NULL , conf.level=0.95){
  
  res <- list()
  
  classification <- classifyADANdf (adanMdl = adan.model , newX = query.md , 
                  newP = query.p , getraw = F  )$dfclass
  
  res$categories = data.frame(categories = apply( classification  , 1, function(x) sum(x,na.rm=T) ))
  
  if( is.null(sdep)){
    sdep <- adan.model$SDEP
  }
  
  # get CI
  res$errorCI <- data.frame( errorCI = qnorm( 1-(1-conf.level)/2) *  sdep * sapply(res$categories[,1] , function(x){
    if(x %in% c(0,1 )) return(1)
    if(x %in% c(2,3 )) return(2)
    return( NA)
  }))
  
  res$classification <- classification
  
  return(res)
}  
  