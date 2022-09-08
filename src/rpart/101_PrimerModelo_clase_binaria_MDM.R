#Arbol elemental con libreria  rpart
#Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

#Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("C:/Users/Marcos/Documents/Maestria/dmeyf_2022")  #Establezco el Working Directory

#cargo el dataset
dataset  <- fread("./datasets/competencia1_2022.csv")

#elimino la variable más importante del arbol (ctrx_quater)
#dataset_alt <- dataset[ , !"ctrx_quarter", with=FALSE]

#para no cambiar todo el scrip, vuelvo a ponerle el nombre original para correr el arbol corto 
#dataset <- dataset_alt

#dataset[ , clase_binaria:= ifelse(clase_ternaria=='CONTINUA',
#                                  'NEG',
#                                  'POS')]

dataset[ , clase:= ifelse(foto_mes=='202101',
                          'enero',
                          'marzo')]

dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dtrain <- dataset
dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo

#genero el modelo max_dept=3,  aqui se construye el arbol
modelo  <- rpart(formula=   "clase ~ . -clase_ternaria - foto_mes",  #quiero predecir clase_ternaria a partir de el resto de las variables
                 data=      dtrain,  #los datos donde voy a entrenar
                 xval=      0,
                 cp=       -0.3,   #esto significa no limitar la complejidad de los splits
                 minsplit=  0,     #minima cantidad de registros para que se haga el split
                 minbucket= 1,     #tamaño minimo de una hoja
                 maxdepth=  5 )    #profundidad maxima del arbol


#grafico el arbol
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)


#aplico el modelo a los datos nuevos
prediccion  <- predict( object= modelo,
                        newdata= dapply,
                        type = "prob")

#prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
#cada columna es el vector de probabilidades 

#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[ , prob_baja2 := prediccion[, "BAJA+2"] ]

#solo le envio estimulo a los registros con probabilidad de BAJA+2 mayor  a  1/40
dapply[ , Predicted := as.numeric( prob_baja2 > 1/40 ) ]

#genero el archivo para Kaggle
#primero creo la carpeta donde va el experimento
dir.create( "./exp/" )
dir.create( "./exp/KA2001" )

fwrite( dapply[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
        file= "./exp/KA2001/K101_001.csv",
        sep=  "," )
