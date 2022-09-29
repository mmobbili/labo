
#limpio la memoria
rm( list=ls() )
gc()

require("data.table")
require("rpart")
require("rpart.plot")

#defino la carpeta donde trabajo
setwd("C:/Users/Marcos/Documents/Maestria/dmeyf_2022")

#cargo el dataset
dataset  <- fread("./datasets/competencia1_2022_ext.csv" )

#creo la clase_binaria donde en la misma bolsa estan los BAJA+1 y BAJA+2
dataset[foto_mes==202101 , clase_binaria := ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]



dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo


modelo  <- rpart(formula=   "clase_binaria ~ . -clase_ternaria",  #quiero predecir clase_ternaria a partir de el resto de las variables
                 data=      dtrain,  #los datos donde voy a entrenar
                 xval=      0,
                 cp=       -0.603,   #esto significa no limitar la complejidad de los splits
                 minsplit=  546,     #minima cantidad de registros para que se haga el split
                 minbucket= 272,     #tamaño minimo de una hoja
                 maxdepth=  20 )    #profundidad maxima del arbol



#aplico el modelo a los datos nuevos
prediccion  <- predict( object=  modelo,
                        newdata= dapply,
                        type = "prob")



#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[ , prob_baja2 := prediccion[, "SI"] ]

#solo le envio estimulo a los registros con probabilidad de BAJA+2 mayor  a  1/40
dapply[ , Predicted := as.numeric( prob_baja2 > 0.060 ) ]


fwrite( dapply[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
        file= "./exp/desper/desp_07_ext.csv",
        sep=  "," )

