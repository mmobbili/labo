#Feature Engineering
#creo nuevas variables dentro del mismo mes
#Condimentar a gusto con nuevas variables

#limpio la memoria
rm( list=ls() )
gc()

require("data.table")


#Establezco el Working Directory
setwd( "C:/Users/Marcos/Documents/Maestria/dmeyf_2022" )


EnriquecerDataset <- function( dataset , arch_destino )
{
  columnas_originales <-  copy(colnames( dataset ))
  
  
  
  
  #------------------------FE---------------
  
  dataset[, f_por_limite_visa := Visa_mlimitecompra/mpayroll]
  #dapply[, f_por_limite_visa := Visa_mlimitecompra/mpayroll]
  
  
  dataset[, f_cprestamos  := cprestamos_personales+cprestamos_prendarios+cprestamos_hipotecarios]
  #dapply[, f_cprestamos  := cprestamos_personales+cprestamos_prendarios+cprestamos_hipotecarios]
  
  
  dataset[, f_mprestamos  := mprestamos_personales+mprestamos_prendarios+mprestamos_hipotecarios]
  #dapply[, f_mprestamos  := mprestamos_personales+mprestamos_prendarios+mprestamos_hipotecarios]
  
  
  dataset[, f_limite_total  := Master_mlimitecompra+Visa_mlimitecompra]
  #dapply[, f_limite_total  := Master_mlimitecompra+Visa_mlimitecompra]
  
  
  dataset[, f_ccompras_total_tj  := Master_cconsumos+Visa_cconsumos]
  #dapply[, f_ccompras_total_tj  := Master_cconsumos+Visa_cconsumos]
  
  
  dataset[, f_mcompras_total_tj  := Master_mconsumospesos+Visa_mconsumospesos]
  #dapply[, f_mcompras_total_tj  := Master_mconsumospesos+Visa_mconsumospesos]
  
  
  dataset[, f_cseguros  := cseguro_vida+cseguro_auto+cseguro_vivienda+cseguro_accidentes_personales]
  #dapply[, f_cseguros  := cseguro_vida+cseguro_auto+cseguro_vivienda+cseguro_accidentes_personales]
  
  
  dataset[, f_cpagos_servicios  := cpagodeservicios+cpagomiscuentas]
  #dapply[, f_cpagos_servicios  := cpagodeservicios+cpagomiscuentas]
  
  
  dataset[, f_mpagos_servicios  := mpagodeservicios+mpagomiscuentas]
  #dapply[, f_mpagos_servicios  := mpagodeservicios+mpagomiscuentas]
  
  
  dataset[, f_ctarjetas_descuentos  := ctarjeta_visa_descuentos+ctarjeta_master_descuentos]
  #dapply[, f_ctarjetas_descuentos  := ctarjeta_visa_descuentos+ctarjeta_master_descuentos]
  
  
  dataset[, f_mtarjetas_descuentos  := mtarjeta_visa_descuentos+mtarjeta_master_descuentos]
  #dapply[, f_mtarjetas_descuentos  := mtarjeta_visa_descuentos+mtarjeta_master_descuentos]
  
  
  dataset[, f_ccomisiones  := ccomisiones_mantenimiento+ccomisiones_otras]
  #dapply[, f_ccomisiones  := ccomisiones_mantenimiento+ccomisiones_otras]
  
  ## Corrijo el data drifting
  
  # corrijo manualmente el drifting de  Visa_fultimo_cierre
  dataset[ (foto_mes==202105 & Visa_fultimo_cierre>= 1), Visa_fultimo_cierre := Visa_fultimo_cierre + 4 ]
  # dataset[ Visa_fultimo_cierre== 7, Visa_fultimo_cierre := 11 ]
  # dataset[ Visa_fultimo_cierre==21, Visa_fultimo_cierre := 25 ]
  # dataset[ Visa_fultimo_cierre==14, Visa_fultimo_cierre := 18 ]
  # dataset[ Visa_fultimo_cierre==28, Visa_fultimo_cierre := 32 ]
  # dataset[ Visa_fultimo_cierre==35, Visa_fultimo_cierre := 39 ]
  # dataset[ Visa_fultimo_cierre> 39, Visa_fultimo_cierre := Visa_fultimo_cierre + 4 ]
  # 
  # corrijo manualmente el drifting de  Visa_fultimo_cierre
  # dapply[ Master_fultimo_cierre== 1, Master_fultimo_cierre :=  4 ]
  # dapply[ Master_fultimo_cierre== 7, Master_fultimo_cierre := 11 ]
  # dapply[ Master_fultimo_cierre==21, Master_fultimo_cierre := 25 ]
  # dapply[ Master_fultimo_cierre==14, Master_fultimo_cierre := 18 ]
  # dapply[ Master_fultimo_cierre==28, Master_fultimo_cierre := 32 ]
  # dapply[ Master_fultimo_cierre==35, Master_fultimo_cierre := 39 ]
  # dapply[ Master_fultimo_cierre> 39, Master_fultimo_cierre := Master_fultimo_cierre + 4 ]
  
  
  
  #INICIO de la seccion donde se deben hacer cambios con variables nuevas
  #se crean los nuevos campos para MasterCard  y Visa, teniendo en cuenta los NA's
  #varias formas de combinar Visa_status y Master_status
  dataset[ , mv_status01       := pmax( Master_status,  Visa_status, na.rm = TRUE) ]
  dataset[ , mv_status02       := Master_status +  Visa_status ]
  dataset[ , mv_status03       := pmax( ifelse( is.na(Master_status), 10, Master_status) , ifelse( is.na(Visa_status), 10, Visa_status) ) ]
  dataset[ , mv_status04       := ifelse( is.na(Master_status), 10, Master_status)  +  ifelse( is.na(Visa_status), 10, Visa_status)  ]
  dataset[ , mv_status05       := ifelse( is.na(Master_status), 10, Master_status)  +  100*ifelse( is.na(Visa_status), 10, Visa_status)  ]

  dataset[ , mv_status06       := ifelse( is.na(Visa_status), 
                                          ifelse( is.na(Master_status), 10, Master_status), 
                                          Visa_status)  ]

  dataset[ , mv_status07       := ifelse( is.na(Master_status), 
                                          ifelse( is.na(Visa_status), 10, Visa_status), 
                                          Master_status)  ]


  #combino MasterCard y Visa
  dataset[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]

  dataset[ , mv_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
  dataset[ , mv_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
  dataset[ , mv_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
  dataset[ , mv_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
  dataset[ , mv_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
  dataset[ , mv_mconsumospesos       := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
  dataset[ , mv_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
  dataset[ , mv_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
  dataset[ , mv_madelantopesos       := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
  dataset[ , mv_madelantodolares     := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
  dataset[ , mv_fultimo_cierre       := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
  dataset[ , mv_mpagado              := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
  dataset[ , mv_mpagospesos          := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
  dataset[ , mv_mpagosdolares        := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
  dataset[ , mv_fechaalta            := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
  dataset[ , mv_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
  dataset[ , mv_cconsumos            := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ]
  dataset[ , mv_cadelantosefectivo   := rowSums( cbind( Master_cadelantosefectivo,  Visa_cadelantosefectivo) , na.rm=TRUE ) ]
  dataset[ , mv_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]

  #a partir de aqui juego con la suma de Mastercard y Visa
  dataset[ , mvr_Master_mlimitecompra:= Master_mlimitecompra / mv_mlimitecompra ]
  dataset[ , mvr_Visa_mlimitecompra  := Visa_mlimitecompra / mv_mlimitecompra ]
  dataset[ , mvr_msaldototal         := mv_msaldototal / mv_mlimitecompra ]
  dataset[ , mvr_msaldopesos         := mv_msaldopesos / mv_mlimitecompra ]
  dataset[ , mvr_msaldopesos2        := mv_msaldopesos / mv_msaldototal ]
  dataset[ , mvr_msaldodolares       := mv_msaldodolares / mv_mlimitecompra ]
  dataset[ , mvr_msaldodolares2      := mv_msaldodolares / mv_msaldototal ]
  dataset[ , mvr_mconsumospesos      := mv_mconsumospesos / mv_mlimitecompra ]
  dataset[ , mvr_mconsumosdolares    := mv_mconsumosdolares / mv_mlimitecompra ]
  dataset[ , mvr_madelantopesos      := mv_madelantopesos / mv_mlimitecompra ]
  dataset[ , mvr_madelantodolares    := mv_madelantodolares / mv_mlimitecompra ]
  dataset[ , mvr_mpagado             := mv_mpagado / mv_mlimitecompra ]
  dataset[ , mvr_mpagospesos         := mv_mpagospesos / mv_mlimitecompra ]
  dataset[ , mvr_mpagosdolares       := mv_mpagosdolares / mv_mlimitecompra ]
  dataset[ , mvr_mconsumototal       := mv_mconsumototal  / mv_mlimitecompra ]
  dataset[ , mvr_mpagominimo         := mv_mpagominimo  / mv_mlimitecompra ]

  #valvula de seguridad para evitar valores infinitos
  #paso los infinitos a NULOS
  infinitos      <- lapply(names(dataset),function(.name) dataset[ , sum(is.infinite(get(.name)))])
  infinitos_qty  <- sum( unlist( infinitos) )
  if( infinitos_qty > 0 )
  {
    cat( "ATENCION, hay", infinitos_qty, "valores infinitos en tu dataset. Seran pasados a NA\n" )
    dataset[mapply(is.infinite, dataset)] <- NA
  }


  #valvula de seguridad para evitar valores NaN  que es 0/0
  #paso los NaN a 0 , decision polemica si las hay
  #se invita a asignar un valor razonable segun la semantica del campo creado
  nans      <- lapply(names(dataset),function(.name) dataset[ , sum(is.nan(get(.name)))])
  nans_qty  <- sum( unlist( nans) )
  if( nans_qty > 0 )
  {
    cat( "ATENCION, hay", nans_qty, "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n" )
    cat( "Si no te gusta la decision, modifica a gusto el programa!\n\n")
    dataset[mapply(is.nan, dataset)] <- 0
  }

  #FIN de la seccion donde se deben hacer cambios con variables nuevas

  columnas_extendidas <-  copy( setdiff(  colnames(dataset), columnas_originales ) )

  #grabo con nombre extendido
  fwrite( dataset,
          file=arch_destino,
          sep= "," )
}
#------------------------------------------------------------------------------

dir.create( "./datasets/",  showWarnings = FALSE)


#lectura rapida del dataset  usando fread  de la libreria  data.table
dataset1  <- fread("./datasets/competencia2_2022.csv.gz")

EnriquecerDataset( dataset1, "./datasets/competencia2_FE_2022.csv.gz" )

#quit( save="no")