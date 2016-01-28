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
adan.analyze <- function( adan.model ) {
  library(reshape2)
  ### cutoff plots
  d2Tr <- adan.model$d2Tr
  d2Trm<- melt(as.data.frame(d2Tr))
  
#   metric_names <- list(
#     "d2cXTr" = "Distance in X centroid",  "d2nXTr" = "Distance to Closest in X" ,
#     "d2mTr"  = "Distance to model" ,  "d2cYTr" = "Distance to Y centroid"  ,
#     "d2nYTr" = "Distance to Closest in Y", "d2nSDEP" = "Distance to local SDEP"
#   )
#   
#   metric_labeller <- function(variable,value){
#       return(metric_names[value])
#     }
  
  dTh <- data.frame(
    variable= c("d2cXTr",  "d2nXTr" , "d2mTr" ,  "d2cYTr"  ,"d2nYTr" , "d2nSDEP"),
    values= c(adan.model$d2cXTh,adan.model$d2nXTh,adan.model$d2mTh,adan.model$d2cYTh,adan.model$d2nYTh,adan.model$d2nSDEPTh)
  )
  metrics <- 
    ggplot(d2Trm)+geom_histogram(aes(value))+
    geom_vline(aes(xintercept = values ),data=dTh)+
    facet_wrap(~ variable , nrow=3,scales = "free" )+theme_bw()+
    ggtitle(paste0( "Metrics and Cutoff at ", adan.model$threshold ))
            
  list(metrics)
}