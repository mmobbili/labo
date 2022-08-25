set.seed( 999979 )

#calcula cuantos encestes logra un jugador con indice de enceste prob
#haciendo qty tiros libres

ftirar  <- function( prob, qty ){
  return( sum( runif(qty) < prob ) )
}

### ronda 1###
#defino los jugadores
mejor      <- 0.7
peloton    <- ( 501:599 ) / 1000
jugadores  <- c( mejor, peloton )

#veo que tiene el vector
jugadores

primero_perdedor <- 0

for( i in 1:10000 ){  #diez mil experimentos

  vaciertos  <- mapply( ftirar, jugadores, 11 )  #11 tiros libres cada jugador

  peor <- which.min( vaciertos )
  diez_peores <- sort(vaciertos)[1:10]
  peores_10 <- which()
#  mejor  <- which.max( vaciertos )
#  if( mejor == 1 )  primero_ganador  <- primero_ganador + 1
  if( peor == 1 )  primero_perdedor  <- primero_perdedor + 1
}

#print(  primero_ganador )
print ( primero_perdedor )



### ronda 2###
#defino los jugadores
mejor      <- 0.7
peloton_2da    <- ( 511:599 ) / 1000
jugadores_2da  <- c( mejor, peloton_2da )

#veo que tiene el vector
jugadores_2da

primero_perdedor <- 0

for( i in 1:10000 ){  #diez mil experimentos
  
  vaciertos  <- mapply( ftirar, jugadores_2da, 15 )  #15 tiros libres cada jugador
  
  peor <- which.min( vaciertos )
  if( peor == 1 )  primero_perdedor  <- primero_perdedor + 1
}

print ( primero_perdedor )


### ronda 3###
#defino los jugadores
mejor      <- 0.7
peloton_3ra    <- ( 521:599 ) / 1000
jugadores_3ra  <- c( mejor, peloton_3ra )

#veo que tiene el vector
jugadores_3ra

primero_perdedor <- 0

for( i in 1:10000 ){  #diez mil experimentos
  
  vaciertos  <- mapply( ftirar, jugadores_3ra, 20 )  #20 tiros libres cada jugador
  
  peor <- which.min( vaciertos )
  #  mejor  <- which.max( vaciertos )
  #  if( mejor == 1 )  primero_ganador  <- primero_ganador + 1
  if( peor == 1 )  primero_perdedor  <- primero_perdedor + 1
}

print ( primero_perdedor )