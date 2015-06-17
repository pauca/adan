classifyADAN <-
function(adriMdl, newX , newP , type="and"){
  dfclass <- classifyADANdf(adriMdl = adriMdl , newX  , newP )$dfclass
  switch( type , 
    and =  apply( dfclass , 1, sum ),
    any = apply( dfclass  , 1, function(x)ifelse( sum(x)==0,0,1 ))
  )
}
