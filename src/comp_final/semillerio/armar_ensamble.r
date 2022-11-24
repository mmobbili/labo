require("data.table")
library(readr)

rm( list=ls() )  #remove all objects
gc()             #garbage collection

setwd("~/Maestria/dmeyf_2022/semillerio/files")

sem1 <- read_delim("./exp_ZZ9410_1_14m_03under_FINAL_pred_01_032.csv", 
  delim = "\t", escape_double = FALSE, 
  col_types = cols(numero_de_cliente = col_skip(), 
  foto_mes = col_skip()), trim_ws = TRUE)


sem2 <- read_delim("./exp_ZZ9410_1_14m_03under_FINAL_pred_02_029.csv", 
                   delim = "\t", escape_double = FALSE, 
                   col_types = cols(numero_de_cliente = col_skip(), 
                                    foto_mes = col_skip()), trim_ws = TRUE)

sem3 <- read_delim("./exp_ZZ9410_1_14m_FINAL_pred_01_046.csv", 
                   delim = "\t", escape_double = FALSE, 
                   col_types = cols(numero_de_cliente = col_skip(), 
                                    foto_mes = col_skip()), trim_ws = TRUE)

sem4 <- read_delim("./exp_ZZ9410_1_14m_FINAL_pred_02_031.csv", 
                   delim = "\t", escape_double = FALSE, 
                   col_types = cols(numero_de_cliente = col_skip(), 
                                    foto_mes = col_skip()), trim_ws = TRUE)

sem5 <- read_delim("./exp_ZZ9410_1_FINAL_pred_01_025.csv", 
                   delim = "\t", escape_double = FALSE, 
                   col_types = cols(numero_de_cliente = col_skip(), 
                                    foto_mes = col_skip()), trim_ws = TRUE)

sem6 <- read_delim("./exp_ZZ9410_1_FINAL_pred_02_026.csv", 
                   delim = "\t", escape_double = FALSE, 
                   col_types = cols(numero_de_cliente = col_skip(), 
                                    foto_mes = col_skip()), trim_ws = TRUE)


sem7 <- read_delim("./exp_ZZ9410_2_14m_03under_FINAL_pred_01_011.csv", 
                   delim = "\t", escape_double = FALSE, 
                   col_types = cols(numero_de_cliente = col_skip(), 
                                    foto_mes = col_skip()), trim_ws = TRUE)

sem8 <- read_delim("./exp_ZZ9410_2_14m_03under_FINAL_pred_02_025.csv", 
                   delim = "\t", escape_double = FALSE, 
                   col_types = cols(numero_de_cliente = col_skip(), 
                                    foto_mes = col_skip()), trim_ws = TRUE)

sem9 <- read_delim("./exp_ZZ9410_2_14m_FINAL_pred_01_054.csv", 
                   delim = "\t", escape_double = FALSE, 
                   col_types = cols(numero_de_cliente = col_skip(), 
                                    foto_mes = col_skip()), trim_ws = TRUE)

sem10 <- read_delim("./exp_ZZ9410_2_14m_FINAL_pred_02_033.csv", 
                   delim = "\t", escape_double = FALSE, 
                   col_types = cols(numero_de_cliente = col_skip(), 
                                    foto_mes = col_skip()), trim_ws = TRUE)


sem11 <- read_delim("./exp_ZZ9410_2_FINAL_pred_01_042.csv", 
                    delim = "\t", escape_double = FALSE, 
                    col_types = cols(numero_de_cliente = col_skip(), 
                                     foto_mes = col_skip()), trim_ws = TRUE)

sem12 <- read_delim("./exp_ZZ9410_2_FINAL_pred_02_007.csv", 
                    delim = "\t", escape_double = FALSE, 
                    col_types = cols(numero_de_cliente = col_skip(), 
                                     foto_mes = col_skip()), trim_ws = TRUE)


sem13 <- read_delim("./exp_ZZ9410_2_14m_03under_FINAL_semi1_pred_01_011.csv", 
                    delim = "\t", escape_double = FALSE, 
                    col_types = cols(numero_de_cliente = col_skip(), 
                                     foto_mes = col_skip()), trim_ws = TRUE)


sem14 <- read_delim("./exp_ZZ9410_2_14m_03under_FINAL_semi2_pred_01_011.csv", 
                    delim = "\t", escape_double = FALSE, 
                    col_types = cols(numero_de_cliente = col_skip(), 
                                     foto_mes = col_skip()), trim_ws = TRUE)


sem15 <- read_delim("./exp_ZZ9410_2_14m_FINAL_semi2_pred_01_054.csv", 
                    delim = "\t", escape_double = FALSE, 
                    col_types = cols(numero_de_cliente = col_skip(), 
                                     foto_mes = col_skip()), trim_ws = TRUE)


sem16 <- read_delim("./exp_ZZ9410_2_14m_FINAL_semi3_pred_01_054.csv", 
                    delim = "\t", escape_double = FALSE, 
                    col_types = cols(numero_de_cliente = col_skip(), 
                                     foto_mes = col_skip()), trim_ws = TRUE)


sem17 <- read_delim("./exp_ZZ9410_1_14m_FINAL_semi4_pred_01_046.csv", 
                    delim = "\t", escape_double = FALSE, 
                    col_types = cols(numero_de_cliente = col_skip(), 
                                     foto_mes = col_skip()), trim_ws = TRUE)


sem18 <- read_delim("./exp_ZZ9410_plus_FINAL_pre_pred_01_011.csv", 
                    delim = "\t", escape_double = FALSE, 
                    col_types = cols(numero_de_cliente = col_skip(), 
                                     foto_mes = col_skip()), trim_ws = TRUE)




nro_cliente <- read_delim("./exp_ZZ9410_1_14m_03under_FINAL_pred_01_032.csv", 
                          delim = "\t", escape_double = FALSE, 
                          col_types = cols(foto_mes = col_skip(), 
                          prob = col_skip()), trim_ws = TRUE)


ensamble<-cbind(nro_cliente, sem1, sem2, sem3, sem4, sem5, sem6, sem7, sem8, sem9, sem10, sem11, sem12, sem13, sem14, sem15, sem16, sem17, sem18)

ensamble$avg_prob = rowMeans(ensamble[,c(2:18)])

ensamble$avg_prob_rank <- frank(-ensamble$avg_prob)


ensamble$envio<-as.numeric(ensamble$avg_prob_rank<10800)

m<-data.frame(ensamble$numero_de_cliente)

m$Predicted<-ensamble$envio

colnames(m)<-c('numero_de_cliente', 'Predicted')

setwd("~/Maestria/dmeyf_2022/semillerio")
fwrite( m, #solo los campos para Kaggle
        file= "./semillero_final.csv",
        sep=  "," )
