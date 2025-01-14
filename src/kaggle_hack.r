#hackeo kaggle
rm( list=ls())
gc()

library( "data.table")   #cargo la libreria  data.table

setwd( "~/buckets/b1/" )  #Aqui se debe poner la ruta de la PC local

dataset <- fread("./datasets/competenciaFINAL_2022.csv.gz")   #cargo el dataset

dataset[  , .N, list( foto_mes, clase_ternaria) ]

dfuturo <-  dataset[ foto_mes==202109 ]

nrow( dfuturo )

vector_ids  <-   dfuturo[ , numero_de_cliente]
head( vector_ids)

vector_enviar <-  rep( 1,  nrow( dfuturo))
length( vector_enviar)

tabla_final  <-   as.data.table(  list(  "numero_de_cliente"= vector_ids,
                                         "Predicted"=         vector_enviar))

head( tabla_final)

#genero el archivo para Kaggle
#creo la carpeta donde va el experimento
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/KH_MDM/", showWarnings = FALSE )

#genero el archivo para Kaggle
fwrite(tabla_final, 
       file= "./exp/KH_MDM/todos_unos.csv",
       sep=  "," )



#Voy a generar una entrega random
vector_enviar_rnd <-  as.numeric(runif(nrow( dfuturo))<=(1000/164935))
length( vector_enviar_rnd)

tabla_final_rnd  <-   as.data.table(  list(  "numero_de_cliente"= vector_ids,
                                         "Predicted"=         vector_enviar_rnd))

head( tabla_final_rnd)

#genero el archivo para Kaggle
#creo la carpeta donde va el experimento
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/KH_MDM/", showWarnings = FALSE )

#genero el archivo para Kaggle
fwrite(tabla_final_rnd, 
       file= "./exp/KH_MDM/unos_rnd.csv",
       sep=  "," )


#Miro como viene evolucionando BAJA+2
library(ggplot2)


bajas_2019<-dataset[clase_ternaria=='BAJA+2' & foto_mes<= '201912' , .N, foto_mes]
bajas_2020<-dataset[clase_ternaria=='BAJA+2' & foto_mes<= '202012' &  foto_mes>= '202001', .N, foto_mes]
bajas_2021<-dataset[clase_ternaria=='BAJA+2' & foto_mes<= '202112' &  foto_mes>= '202101', .N, foto_mes]



# basic scatterplot
ggplot(pedo, aes(x=foto_mes, y=N)) + 
  geom_point()



plot( density( dataset[ foto_mes==202101, ctrx_quarter] ) )

dataset[ foto_mes==202101, ctrx_quarter]
