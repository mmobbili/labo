rm( list=ls() )
gc()

require("data.table")

#Establezco el Working Directory
setwd( "C:/Users/Marcos/Documents/Maestria/dmeyf_2022" )


#lectura rapida del dataset  usando fread  de la libreria  data.table
dataset3  <- fread("./datasets/competencia3_2022.csv.gz")

#filtro pocos registros para ver que corra localmente
dataset3_ch <- dataset3[ sample(.N, 20000)]

dataset3_med <- dataset3[ sample(.N, 100000)]


#grabo con nombre extendido
fwrite( dataset3_ch,
        file="./datasets/competencia3_local_2022.csv.gz",
        sep= "," )


fwrite( dataset3_med,
        file="./datasets/competencia3_local_med_2022.csv.gz",
        sep= "," )



pedo  <- fread("./datasets/competencia2_small_2022.csv.gz")



########
dt<-as.data.table(mtcars)

set.seed(6)
ds_sampleado_1 <- dt[ , .SD[sample(.N, 5)] , by = cyl ]
ds_sampleado_2 <- dt[ sample(.N, 5), colnames(dt), by = cyl ]


dt_1<-dt[, .SD, by=cyl]
