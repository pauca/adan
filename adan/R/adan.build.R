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

adan.build <- function( train.md , train.a , train.p , 
                        scale.md = F){
  buildADAN (
      X= train.md , Y= train.a , P=train.p  , 
      scale = scale.md , ncomp = NULL , 
      explvar = 80 , threshold = 0.95
    )
}