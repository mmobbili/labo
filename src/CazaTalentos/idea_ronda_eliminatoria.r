#La idea es hacer rondas de tiros libres donde los peores se van eliminando.
# Supongo que incialmente son necesarios menos tiros libres para encontrar los peores
# A medida que se van eliminando y los que quedan son los menos peores del montón, necesito mayor 
# cantidad de tiros libres para distinguir a los peores
# El esquema propuesto es:

#ronda	# jugadores	# tiros/jug	# tiros en ronda
#1	100	10	1000
#2	90	15	1350
#3	80	20	1600
#4	70	25	1750
#5	60	30	1800
#6	50	35	1750
#7	40	35	1400
#8	30	35	1050
#9	20	35	700
#10	10	40	400
#11	8	45	360
#12	6	50	300
#13	4	70	280
#14	2	80	160
# tiros totales	13900



set.seed( 999978 )

#calcula cuantos encestes logra un jugador con indice de enceste prob
#haciendo qty tiros libres

ftirar  <- function( prob, qty ){
  return( sum( runif(qty) < prob ) )
}

###Primera ronda####

#defino los jugadores
mejor      <- 0.7
peloton    <- ( 501:599 ) / 1000
jugadores  <- c( mejor, peloton )

#veo que tiene el vector
jugadores


#diez_peores <- sort(aciertos)[1:10]
#diez_peores
#which(aciertos %in% diez_peores)

#primero_ganador  <- 0
lista_peores <- c()

for( i in 1:10000 ){  #10000 experimentos
  
  vaciertos  <- mapply( ftirar, jugadores, 15 )  #11 tiros libres cada jugador
  
  diez_peores_scores <- sort(vaciertos)[1:10]
  diez_peores_jugadores <- which(vaciertos %in% diez_peores_scores)
  lista_peores <- append(lista_peores, diez_peores_jugadores)
  
  #mejor  <- which.max( vaciertos )
  #if( mejor == 1 )  primero_ganador  <- primero_ganador + 1
}

peores_df<-data.frame(table(lista_peores))
head(peores_df)
elim_1ra <- tail(peores_df[order(peores_df$Freq),],10)   #los 10 que más aparecen en los experimentos, los que quedan afuera en esta ronda
elim_1ra



###Segunda ronda####
# defino los jugadores
# Elimino a los 10 que definí en la fila 72
jugadores_2da<-jugadores[-(as.numeric(elim_1ra$lista_peores))]
jugadores_2da

diez_peores_scores_2da <- c()
diez_peores_jugadores_2da <- c()
lista_peores_2da <- c()

for( i in 1:10000 ){  #10000 experimentos
  
  vaciertos  <- mapply( ftirar, jugadores_2da, 15 )  #15 tiros libres cada jugador
  
  diez_peores_scores_2da <- sort(vaciertos)[1:10]
  diez_peores_jugadores_2da <- which(vaciertos %in% diez_peores_scores_2da)
  lista_peores_2da <- append(lista_peores_2da, diez_peores_jugadores_2da)
}

peores_2da_df<-data.frame(table(lista_peores_2da))
head(peores_2da_df)
elim_2da <- tail(peores_2da_df[order(peores_2da_df$Freq),],10)   #los 10 que más aparecen en los experimentos, los que quedan afuera en esta ronda
elim_2da


###Tercera ronda####
# defino los jugadores
# Elimino a los 10 que definí en la 2da ronda
jugadores_3ra<-jugadores_2da[-(as.numeric(elim_2da$lista_peores))]
jugadores_3ra

diez_peores_scores_3ra <- c()
diez_peores_jugadores_3ra <- c()
lista_peores_3ra <- c()

for( i in 1:10000 ){  #10000 experimentos
  
  vaciertos  <- mapply( ftirar, jugadores_3ra, 20 )  #20 tiros libres cada jugador
  
  diez_peores_scores_3ra <- sort(vaciertos)[1:10]
  diez_peores_jugadores_3ra <- which(vaciertos %in% diez_peores_scores_3ra)
  lista_peores_3ra <- append(lista_peores_3ra, diez_peores_jugadores_3ra)
}

peores_3ra_df<-data.frame(table(lista_peores_3ra))
head(peores_3ra_df)
elim_3ra <- tail(peores_3ra_df[order(peores_3ra_df$Freq),],10)   #los 10 que más aparecen en los experimentos, los que quedan afuera en esta ronda
elim_3ra


###Cuarta ronda####
# defino los jugadores
# Elimino a los 10 que definí en la 3ra ronda
jugadores_4ta<-jugadores_3ra[-(as.numeric(elim_3ra$lista_peores))]
jugadores_4ta

diez_peores_scores_4ta <- c()
diez_peores_jugadores_4ta <- c()
lista_peores_4ta <- c()

for( i in 1:10000 ){  #10000 experimentos
  
  vaciertos  <- mapply( ftirar, jugadores_4ta, 25 )  #25 tiros libres cada jugador
  
  diez_peores_scores_4ta <- sort(vaciertos)[1:10]
  diez_peores_jugadores_4ta <- which(vaciertos %in% diez_peores_scores_4ta)
  lista_peores_4ta <- append(lista_peores_4ta, diez_peores_jugadores_4ta)
}

peores_4ta_df<-data.frame(table(lista_peores_4ta))
head(peores_4ta_df)
elim_4ta <- tail(peores_4ta_df[order(peores_4ta_df$Freq),],10)   #los 10 que más aparecen en los experimentos, los que quedan afuera en esta ronda
elim_4ta
jugadores_ant <- jugadores_4ta
elim_ant <- elim_4ta

###Quinta ronda####
# defino los jugadores
# Elimino a los 10 que definí en ronda anterior
jugadores_new <- jugadores_ant[-(as.numeric(elim_ant$lista_peores))]
jugadores_new

diez_peores_scores_new <- c()
diez_peores_jugadores_new <- c()
lista_peores_new <- c()

for( i in 1:10000 ){  #10000 experimentos
  
  vaciertos  <- mapply( ftirar, jugadores_new, 30 )  #30 tiros libres cada jugador
  
  diez_peores_scores_new <- sort(vaciertos)[1:10]
  diez_peores_jugadores_new <- which(vaciertos %in% diez_peores_scores_new)
  lista_peores_new <- append(lista_peores_new, diez_peores_jugadores_new)
}

peores_new_df<-data.frame(table(lista_peores_new))
head(peores_new_df)
elim_new <- tail(peores_new_df[order(peores_new_df$Freq),],10)   #los 10 que más aparecen en los experimentos, los que quedan afuera en esta ronda
elim_new


###Sexta ronda####
jugadores_ant <- jugadores_new
elim_ant <- elim_new
rm(peores_new_df)

# defino los jugadores
# Elimino a los 10 que definí en ronda anterior
jugadores_new <- jugadores_ant[-(as.numeric(elim_ant$lista_peores))]
jugadores_new

diez_peores_scores_new <- c()
diez_peores_jugadores_new <- c()
lista_peores_new <- c()

for( i in 1:10000 ){  #10000 experimentos
  
  vaciertos  <- mapply( ftirar, jugadores_new, 35 )  #35 tiros libres cada jugador
  
  diez_peores_scores_new <- sort(vaciertos)[1:10]
  diez_peores_jugadores_new <- which(vaciertos %in% diez_peores_scores_new)
  lista_peores_new <- append(lista_peores_new, diez_peores_jugadores_new)
}

peores_new_df<-data.frame(table(lista_peores_new))
head(peores_new_df)
elim_new <- tail(peores_new_df[order(peores_new_df$Freq),],10)   #los 10 que más aparecen en los experimentos, los que quedan afuera en esta ronda
elim_new


###Septima ronda####
jugadores_ant <- jugadores_new
elim_ant <- elim_new
rm(peores_new_df)

# defino los jugadores
# Elimino a los 10 que definí en ronda anterior
jugadores_new <- jugadores_ant[-(as.numeric(elim_ant$lista_peores))]
jugadores_new

diez_peores_scores_new <- c()
diez_peores_jugadores_new <- c()
lista_peores_new <- c()

for( i in 1:10000 ){  #10000 experimentos
  
  vaciertos  <- mapply( ftirar, jugadores_new, 35 )  #35 tiros libres cada jugador
  
  diez_peores_scores_new <- sort(vaciertos)[1:10]
  diez_peores_jugadores_new <- which(vaciertos %in% diez_peores_scores_new)
  lista_peores_new <- append(lista_peores_new, diez_peores_jugadores_new)
}

peores_new_df<-data.frame(table(lista_peores_new))
head(peores_new_df)
elim_new <- tail(peores_new_df[order(peores_new_df$Freq),],10)   #los 10 que más aparecen en los experimentos, los que quedan afuera en esta ronda
elim_new


###Octava ronda####
jugadores_ant <- jugadores_new
elim_ant <- elim_new
rm(peores_new_df)

# defino los jugadores
# Elimino a los 10 que definí en ronda anterior
jugadores_new <- jugadores_ant[-(as.numeric(elim_ant$lista_peores))]
jugadores_new

diez_peores_scores_new <- c()
diez_peores_jugadores_new <- c()
lista_peores_new <- c()

for( i in 1:10000 ){  #10000 experimentos
  
  vaciertos  <- mapply( ftirar, jugadores_new, 35 )  #35 tiros libres cada jugador
  
  diez_peores_scores_new <- sort(vaciertos)[1:10]
  diez_peores_jugadores_new <- which(vaciertos %in% diez_peores_scores_new)
  lista_peores_new <- append(lista_peores_new, diez_peores_jugadores_new)
}

peores_new_df<-data.frame(table(lista_peores_new))
head(peores_new_df)
elim_new <- tail(peores_new_df[order(peores_new_df$Freq),],10)   #los 10 que más aparecen en los experimentos, los que quedan afuera en esta ronda
elim_new

###Novena ronda####
jugadores_ant <- jugadores_new
elim_ant <- elim_new
rm(peores_new_df)

# defino los jugadores
# Elimino a los 10 que definí en ronda anterior
jugadores_new <- jugadores_ant[-(as.numeric(elim_ant$lista_peores))]
jugadores_new

diez_peores_scores_new <- c()
diez_peores_jugadores_new <- c()
lista_peores_new <- c()

for( i in 1:10000 ){  #10000 experimentos
  
  vaciertos  <- mapply( ftirar, jugadores_new, 35 )  #35 tiros libres cada jugador
  
  diez_peores_scores_new <- sort(vaciertos)[1:10]
  diez_peores_jugadores_new <- which(vaciertos %in% diez_peores_scores_new)
  lista_peores_new <- append(lista_peores_new, diez_peores_jugadores_new)
}

peores_new_df<-data.frame(table(lista_peores_new))
head(peores_new_df)
elim_new <- tail(peores_new_df[order(peores_new_df$Freq),],10)   #los 10 que más aparecen en los experimentos, los que quedan afuera en esta ronda
elim_new


###Décima ronda####
jugadores_ant <- jugadores_new
elim_ant <- elim_new
rm(peores_new_df)

# defino los jugadores
# Elimino a los 10 que definí en ronda anterior
jugadores_new <- jugadores_ant[-(as.numeric(elim_ant$lista_peores))]
jugadores_new

dos_peores_scores_new <- c()
dos_peores_jugadores_new <- c()
lista_peores_new <- c()

for( i in 1:10000 ){  #10000 experimentos
  
  vaciertos  <- mapply( ftirar, jugadores_new, 40 )  #40 tiros libres cada jugador
  
  dos_peores_scores_new <- sort(vaciertos)[1:2]
  dos_peores_jugadores_new <- which(vaciertos %in% dos_peores_scores_new)
  lista_peores_new <- append(lista_peores_new, dos_peores_jugadores_new)
}

peores_new_df<-data.frame(table(lista_peores_new))
head(peores_new_df)
elim_new <- tail(peores_new_df[order(peores_new_df$Freq),],2)   #los 2 que más aparecen en los experimentos, los que quedan afuera en esta ronda
elim_new


###Undécima ronda####
jugadores_ant <- jugadores_new
elim_ant <- elim_new
rm(peores_new_df)

# defino los jugadores
# Elimino a los 2 que definí en ronda anterior
jugadores_new <- jugadores_ant[-(as.numeric(elim_ant$lista_peores))]
jugadores_new

dos_peores_scores_new <- c()
dos_peores_jugadores_new <- c()
lista_peores_new <- c()

for( i in 1:10000 ){  #10000 experimentos
  
  vaciertos  <- mapply( ftirar, jugadores_new, 45 )  #45 tiros libres cada jugador
  
  dos_peores_scores_new <- sort(vaciertos)[1:2]
  dos_peores_jugadores_new <- which(vaciertos %in% dos_peores_scores_new)
  lista_peores_new <- append(lista_peores_new, dos_peores_jugadores_new)
}

peores_new_df<-data.frame(table(lista_peores_new))
head(peores_new_df)
elim_new <- tail(peores_new_df[order(peores_new_df$Freq),],2)   #los 2 que más aparecen en los experimentos, los que quedan afuera en esta ronda
elim_new


###Duodécima ronda####
jugadores_ant <- jugadores_new
elim_ant <- elim_new
rm(peores_new_df)

# defino los jugadores
# Elimino a los 2 que definí en ronda anterior
jugadores_new <- jugadores_ant[-(as.numeric(elim_ant$lista_peores))]
jugadores_new

dos_peores_scores_new <- c()
dos_peores_jugadores_new <- c()
lista_peores_new <- c()

for( i in 1:10000 ){  #10000 experimentos
  
  vaciertos  <- mapply( ftirar, jugadores_new, 50 )  #50 tiros libres cada jugador
  
  dos_peores_scores_new <- sort(vaciertos)[1:2]
  dos_peores_jugadores_new <- which(vaciertos %in% dos_peores_scores_new)
  lista_peores_new <- append(lista_peores_new, dos_peores_jugadores_new)
}

peores_new_df<-data.frame(table(lista_peores_new))
head(peores_new_df)
elim_new <- tail(peores_new_df[order(peores_new_df$Freq),],2)   #los 2 que más aparecen en los experimentos, los que quedan afuera en esta ronda
elim_new


###13va ronda####
jugadores_ant <- jugadores_new
elim_ant <- elim_new
rm(peores_new_df)

# defino los jugadores
# Elimino a los 2 que definí en ronda anterior
jugadores_new <- jugadores_ant[-(as.numeric(elim_ant$lista_peores))]
jugadores_new

dos_peores_scores_new <- c()
dos_peores_jugadores_new <- c()
lista_peores_new <- c()

for( i in 1:10000 ){  #10000 experimentos
  
  vaciertos  <- mapply( ftirar, jugadores_new, 70 )  #70 tiros libres cada jugador
  
  dos_peores_scores_new <- sort(vaciertos)[1:2]
  dos_peores_jugadores_new <- which(vaciertos %in% dos_peores_scores_new)
  lista_peores_new <- append(lista_peores_new, dos_peores_jugadores_new)
}

peores_new_df<-data.frame(table(lista_peores_new))
head(peores_new_df)
elim_new <- tail(peores_new_df[order(peores_new_df$Freq),],2)   #los 2 que más aparecen en los experimentos, los que quedan afuera en esta ronda
elim_new



###Final round####

jugadores_ant <- jugadores_new
elim_ant <- elim_new
rm(peores_new_df)

# defino los jugadores
# Elimino a los 2 que definí en ronda anterior
jugadores_new <- jugadores_ant[-(as.numeric(elim_ant$lista_peores))]
jugadores_new

primero_ganador  <- 0

for( i in 1:10000 ){  #10000 experimentos
  
  vaciertos  <- mapply( ftirar, jugadores_new, 300 )  #80 tiros libres cada jugador
  
  mejor  <- which.max( vaciertos )
  if( mejor == 1 )  primero_ganador  <- primero_ganador + 1
  
}

print(primero_ganador)

