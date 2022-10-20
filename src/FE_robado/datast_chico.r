rm( list=ls() )
gc()

require("data.table")

#Establezco el Working Directory
setwd( "C:/Users/Marcos/Documents/Maestria/dmeyf_2022" )


#lectura rapida del dataset  usando fread  de la libreria  data.table
dataset1  <- fread("./datasets/competencia2_2022.csv.gz")

#filtro pocos registros para ver que corra localmente
dataset1 <- dataset1[ sample(.N, 50000)]


#grabo con nombre extendido
fwrite( dataset1,
        file="./datasets/competencia2_small_2022.csv.gz",
        sep= "," )



pedo  <- fread("./datasets/competencia2_small_2022.csv.gz")
