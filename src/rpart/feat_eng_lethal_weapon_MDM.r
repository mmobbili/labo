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

dataset[ , campo1 := as.integer( ctrx_quarter <14 & active_quarter == 0 & mcuentas_saldo < -1558.3 & cdescubierto_preacordado == 0 ) ]
dataset[ , campo2 := as.integer( ctrx_quarter <14 & active_quarter == 0 & mcuentas_saldo < -1558.3 & cdescubierto_preacordado == 1 ) ]
dataset[ , campo3 := as.integer( ctrx_quarter <14 & active_quarter == 0 & mcuentas_saldo >= -1558.3 & cdescubierto_preacordado == 0 ) ]
dataset[ , campo4 := as.integer( ctrx_quarter <14 & active_quarter == 0 & mcuentas_saldo < -1558.3 & cdescubierto_preacordado == 1 ) ]

dataset[ , campo5 := as.integer( ctrx_quarter <14 & active_quarter == 1 & Visa_status >= 8 & mprestamos_personales < 13879 ) ]
dataset[ , campo6 := as.integer( ctrx_quarter <14 & active_quarter == 1 & Visa_status >= 8 & mprestamos_personales >= 13879 ) ]

dataset[ , campo7 := as.integer( ctrx_quarter <14 & active_quarter == 1 & Visa_status < 8 & mpasivos_margen < 112.5 ) ]
dataset[ , campo8 := as.integer( ctrx_quarter <14 & active_quarter == 1 & Visa_status < 8 & mpasivos_margen >= 112.5 ) ]

fwrite(dataset, file="./datasets/pedo.csv")

##Ejemplos de Gustavo:
#~~~
#dataset[ , campo1 := as.integer( ctrx_quarter <14 & mcuentas_saldo < -1256.1 & cprestamos_personales <2 ) ]
# dataset[ , campo2 := as.integer( ctrx_quarter <14 & mcuentas_saldo < -1256.1 & cprestamos_personales>=2 ) ]
# 
# dataset[ , campo3 := as.integer( ctrx_quarter <14 & mcuentas_saldo>= -1256.1 & mcaja_ahorro <2601.1 ) ]
# dataset[ , campo4 := as.integer( ctrx_quarter <14 & mcuentas_saldo>= -1256.1 & mcaja_ahorro>=2601.1 ) ]
# 
# dataset[ , campo5 := as.integer( ctrx_quarter>=14 & ( Visa_status>=8 | is.na(Visa_status) ) & ( Master_status>=8 | is.na(Master_status) ) ) ]
# dataset[ , campo6 := as.integer( ctrx_quarter>=14 & ( Visa_status>=8 | is.na(Visa_status) ) & ( Master_status <8 & !is.na(Master_status) ) ) ]
# 
# dataset[ , campo7 := as.integer( ctrx_quarter>=14 & Visa_status <8 & !is.na(Visa_status) & ctrx_quarter <38 ) ]
# dataset[ , campo8 := as.integer( ctrx_quarter>=14 & Visa_status <8 & !is.na(Visa_status) & ctrx_quarter>=38 ) ]
# ~~~