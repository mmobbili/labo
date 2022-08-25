set.seed( 999978 )

#calcula cuantos encestes logra un jugador con indice de enceste prob
#haciendo qty tiros libres

ftirar  <- function( prob, qty ){
  return( sum( runif(qty) < prob ) )
}

#defino los jugadores
mejor      <- 0.7
peloton    <- ( 501:599 ) / 1000
jugadores  <- c(mejor, peloton)

