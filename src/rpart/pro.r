#limpio la memoria
rm( list=ls() )
gc()

require("data.table")
require("rpart")
require("rpart.plot")


library(readxl)


#defino la carpeta donde trabajo
setwd("C:/Users/Marcos/Documents/Maestria/dmeyf_2022")


DiccionarioDatos <- read_excel("./DiccionarioDatos.xlsx")

#cargo el dataset
dataset  <- fread("./datasets/competencia1_2022.csv" )

pru <- dataset[, 1:10]

ds_ch <- dataset[1:20,1:4]
ds_ch[, active_quarter:=1:20]
ds_ch[, cliente_vip:=NULL]
ds_ch[, index1:=1]
ds_ch[, index2:=2]
ds_ch[, index3:=3]
ds_ch[, index4:=4]
ds_ch[, index5:=5]


for (i in 3:7)
{for (j in (i+1):8)
  {ds_ch[ , paste0("FE",i,j) := ds_ch[ , ..i] * ds_ch[ , ..j] ]
  }
}


for (i in 12:154)
{dataset[ , paste0("FE11_",i) := dataset[,11]*dataset[,..i] ]}


for (i in 120:153)
{for (j in (i+1):154)
{dataset[ , paste0("FE",i,j) := dataset[ , ..i] * dataset[ , ..j] ]
}
}

fwrite( dataset, file="./datasets/competencia1_2022_ext_FEloco.csv")
