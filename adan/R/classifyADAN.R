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
classifyADAN <-
function(adriMdl, newX , newP , type="and"){
  dfclass <- classifyADANdf(adriMdl = adriMdl , newX  , newP )$dfclass
  switch( type , 
    and =  apply( dfclass , 1, sum ),
    any = apply( dfclass  , 1, function(x)ifelse( sum(x)==0,0,1 ))
  )
}
