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
  
  
  # Clases BAJAS+1 y BAJA+2 combinadas
  dataset[, clase_binaria2 := ifelse(
    clase_ternaria == "CONTINUA",
    "nada",
    "mando"
  )]
  
  dtrain  <- dataset[ foto_mes==202103 ]  #defino donde voy a entrenar
  dapply  <- dataset[ foto_mes==202105 ]  #defino donde voy a aplicar el modelo
  
  
  #------------------------FE---------------
  
  dtrain[, f_por_limite_visa := Visa_mlimitecompra/mpayroll]
  dapply[, f_por_limite_visa := Visa_mlimitecompra/mpayroll]
  
  
  dtrain[, f_cprestamos  := cprestamos_personales+cprestamos_prendarios+cprestamos_hipotecarios]
  dapply[, f_cprestamos  := cprestamos_personales+cprestamos_prendarios+cprestamos_hipotecarios]
  
  
  dtrain[, f_mprestamos  := mprestamos_personales+mprestamos_prendarios+mprestamos_hipotecarios]
  dapply[, f_mprestamos  := mprestamos_personales+mprestamos_prendarios+mprestamos_hipotecarios]
  
  
  dtrain[, f_limite_total  := Master_mlimitecompra+Visa_mlimitecompra]
  dapply[, f_limite_total  := Master_mlimitecompra+Visa_mlimitecompra]
  
  
  dtrain[, f_ccompras_total_tj  := Master_cconsumos+Visa_cconsumos]
  dapply[, f_ccompras_total_tj  := Master_cconsumos+Visa_cconsumos]
  
  
  dtrain[, f_mcompras_total_tj  := Master_mconsumospesos+Visa_mconsumospesos]
  dapply[, f_mcompras_total_tj  := Master_mconsumospesos+Visa_mconsumospesos]
  
  
  dtrain[, f_cseguros  := cseguro_vida+cseguro_auto+cseguro_vivienda+cseguro_accidentes_personales]
  dapply[, f_cseguros  := cseguro_vida+cseguro_auto+cseguro_vivienda+cseguro_accidentes_personales]
  
  
  dtrain[, f_cpagos_servicios  := cpagodeservicios+cpagomiscuentas]
  dapply[, f_cpagos_servicios  := cpagodeservicios+cpagomiscuentas]
  
  
  dtrain[, f_mpagos_servicios  := mpagodeservicios+mpagomiscuentas]
  dapply[, f_mpagos_servicios  := mpagodeservicios+mpagomiscuentas]
  
  
  dtrain[, f_ctarjetas_descuentos  := ctarjeta_visa_descuentos+ctarjeta_master_descuentos]
  dapply[, f_ctarjetas_descuentos  := ctarjeta_visa_descuentos+ctarjeta_master_descuentos]
  
  
  dtrain[, f_mtarjetas_descuentos  := mtarjeta_visa_descuentos+mtarjeta_master_descuentos]
  dapply[, f_mtarjetas_descuentos  := mtarjeta_visa_descuentos+mtarjeta_master_descuentos]
  
  
  dtrain[, f_ccomisiones  := ccomisiones_mantenimiento+ccomisiones_otras]
  dapply[, f_ccomisiones  := ccomisiones_mantenimiento+ccomisiones_otras]
  
  ## Corrijo el data drifting
  
  # corrijo manualmente el drifting de  Visa_fultimo_cierre
  dapply[ Visa_fultimo_cierre== 1, Visa_fultimo_cierre :=  4 ]
  dapply[ Visa_fultimo_cierre== 7, Visa_fultimo_cierre := 11 ]
  dapply[ Visa_fultimo_cierre==21, Visa_fultimo_cierre := 25 ]
  dapply[ Visa_fultimo_cierre==14, Visa_fultimo_cierre := 18 ]
  dapply[ Visa_fultimo_cierre==28, Visa_fultimo_cierre := 32 ]
  dapply[ Visa_fultimo_cierre==35, Visa_fultimo_cierre := 39 ]
  dapply[ Visa_fultimo_cierre> 39, Visa_fultimo_cierre := Visa_fultimo_cierre + 4 ]
  
  # corrijo manualmente el drifting de  Visa_fultimo_cierre
  dapply[ Master_fultimo_cierre== 1, Master_fultimo_cierre :=  4 ]
  dapply[ Master_fultimo_cierre== 7, Master_fultimo_cierre := 11 ]
  dapply[ Master_fultimo_cierre==21, Master_fultimo_cierre := 25 ]
  dapply[ Master_fultimo_cierre==14, Master_fultimo_cierre := 18 ]
  dapply[ Master_fultimo_cierre==28, Master_fultimo_cierre := 32 ]
  dapply[ Master_fultimo_cierre==35, Master_fultimo_cierre := 39 ]
  dapply[ Master_fultimo_cierre> 39, Master_fultimo_cierre := Master_fultimo_cierre + 4 ]
  
  
  
  #INICIO de la seccion donde se deben hacer cambios con variables nuevas
  #se crean los nuevos campos para MasterCard  y Visa, teniendo en cuenta los NA's
  #varias formas de combinar Visa_status y Master_status
  
  dtrain[ , mv_status01       := pmax( Master_status,  Visa_status, na.rm = TRUE) ]
  dapply[ , mv_status01       := pmax( Master_status,  Visa_status, na.rm = TRUE) ]
  
  dtrain[ , mv_status02       := Master_status +  Visa_status ]
  dapply[ , mv_status02       := Master_status +  Visa_status ]
  
  dtrain[ , mv_status03       := pmax( ifelse( is.na(Master_status), 10, Master_status) , ifelse( is.na(Visa_status), 10, Visa_status) ) ]
  dapply[ , mv_status03       := pmax( ifelse( is.na(Master_status), 10, Master_status) , ifelse( is.na(Visa_status), 10, Visa_status) ) ]
  
  dtrain[ , mv_status04       := ifelse( is.na(Master_status), 10, Master_status)  +  ifelse( is.na(Visa_status), 10, Visa_status)  ]
  dapply[ , mv_status04       := ifelse( is.na(Master_status), 10, Master_status)  +  ifelse( is.na(Visa_status), 10, Visa_status)  ]
  
  dtrain[ , mv_status05       := ifelse( is.na(Master_status), 10, Master_status)  +  100*ifelse( is.na(Visa_status), 10, Visa_status)  ]
  dapply[ , mv_status05       := ifelse( is.na(Master_status), 10, Master_status)  +  100*ifelse( is.na(Visa_status), 10, Visa_status)  ]

  dtrain[ , mv_status06       := ifelse( is.na(Visa_status), 
                                          ifelse( is.na(Master_status), 10, Master_status), 
                                          Visa_status)  ]
  dapply[ , mv_status06       := ifelse( is.na(Visa_status), 
                                          ifelse( is.na(Master_status), 10, Master_status), 
                                          Visa_status)  ]

  dtrain[ , mv_status07       := ifelse( is.na(Master_status), 
                                          ifelse( is.na(Visa_status), 10, Visa_status), 
                                          Master_status)  ]
  dapply[ , mv_status07       := ifelse( is.na(Master_status), 
                                          ifelse( is.na(Visa_status), 10, Visa_status), 
                                          Master_status)  ]


  #combino MasterCard y Visa
  dtrain[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]
  dapply[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]

  dtrain[ , mv_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
  dapply[ , mv_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
  
  dtrain[ , mv_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
  dapply[ , mv_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
  
  dtrain[ , mv_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
  dapply[ , mv_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
  
  dtrain[ , mv_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
  dapply[ , mv_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
  
  dtrain[ , mv_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
  dapply[ , mv_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
  
  dtrain[ , mv_mconsumospesos       := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
  dapply[ , mv_mconsumospesos       := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
  
  dtrain[ , mv_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
  dapply[ , mv_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
  
  dtrain[ , mv_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
  dapply[ , mv_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
  
  dtrain[ , mv_madelantopesos       := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
  dapply[ , mv_madelantopesos       := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
  
  dtrain[ , mv_madelantodolares     := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
  dapply[ , mv_madelantodolares     := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
  
  dtrain[ , mv_fultimo_cierre       := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
  dapply[ , mv_fultimo_cierre       := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
  
  dtrain[ , mv_mpagado              := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
  dapply[ , mv_mpagado              := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
  
  dtrain[ , mv_mpagospesos          := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
  dapply[ , mv_mpagospesos          := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
  
  dtrain[ , mv_mpagosdolares        := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
  dapply[ , mv_mpagosdolares        := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
  
  dtrain[ , mv_fechaalta            := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
  dapply[ , mv_fechaalta            := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
  
  dtrain[ , mv_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
  dapply[ , mv_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
  
  dtrain[ , mv_cconsumos            := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ]
  dapply[ , mv_cconsumos            := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ]
  
  dtrain[ , mv_cadelantosefectivo   := rowSums( cbind( Master_cadelantosefectivo,  Visa_cadelantosefectivo) , na.rm=TRUE ) ]
  dapply[ , mv_cadelantosefectivo   := rowSums( cbind( Master_cadelantosefectivo,  Visa_cadelantosefectivo) , na.rm=TRUE ) ]
  
  dtrain[ , mv_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]
  dapply[ , mv_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]

  #a partir de aqui juego con la suma de Mastercard y Visa
  dtrain[ , mvr_Master_mlimitecompra:= Master_mlimitecompra / mv_mlimitecompra ]
  dapply[ , mvr_Master_mlimitecompra:= Master_mlimitecompra / mv_mlimitecompra ]
  
  dtrain[ , mvr_Visa_mlimitecompra  := Visa_mlimitecompra / mv_mlimitecompra ]
  dapply[ , mvr_Visa_mlimitecompra  := Visa_mlimitecompra / mv_mlimitecompra ]
  
  dtrain[ , mvr_msaldototal         := mv_msaldototal / mv_mlimitecompra ]
  dapply[ , mvr_msaldototal         := mv_msaldototal / mv_mlimitecompra ]
  
  dtrain[ , mvr_msaldopesos         := mv_msaldopesos / mv_mlimitecompra ]
  dapply[ , mvr_msaldopesos         := mv_msaldopesos / mv_mlimitecompra ]
  
  dtrain[ , mvr_msaldopesos2        := mv_msaldopesos / mv_msaldototal ]
  dapply[ , mvr_msaldopesos2        := mv_msaldopesos / mv_msaldototal ]
  
  dtrain[ , mvr_msaldodolares       := mv_msaldodolares / mv_mlimitecompra ]
  dapply[ , mvr_msaldodolares       := mv_msaldodolares / mv_mlimitecompra ]
  
  dtrain[ , mvr_msaldodolares2      := mv_msaldodolares / mv_msaldototal ]
  dapply[ , mvr_msaldodolares2      := mv_msaldodolares / mv_msaldototal ]
  
  dtrain[ , mvr_mconsumospesos      := mv_mconsumospesos / mv_mlimitecompra ]
  dapply[ , mvr_mconsumospesos      := mv_mconsumospesos / mv_mlimitecompra ]
  
  dtrain[ , mvr_mconsumosdolares    := mv_mconsumosdolares / mv_mlimitecompra ]
  dapply[ , mvr_mconsumosdolares    := mv_mconsumosdolares / mv_mlimitecompra ]
  
  dtrain[ , mvr_madelantopesos      := mv_madelantopesos / mv_mlimitecompra ]
  dapply[ , mvr_madelantopesos      := mv_madelantopesos / mv_mlimitecompra ]
  
  dtrain[ , mvr_madelantodolares    := mv_madelantodolares / mv_mlimitecompra ]
  dapply[ , mvr_madelantodolares    := mv_madelantodolares / mv_mlimitecompra ]
  
  dtrain[ , mvr_mpagado             := mv_mpagado / mv_mlimitecompra ]
  dapply[ , mvr_mpagado             := mv_mpagado / mv_mlimitecompra ]
  
  dtrain[ , mvr_mpagospesos         := mv_mpagospesos / mv_mlimitecompra ]
  dapply[ , mvr_mpagospesos         := mv_mpagospesos / mv_mlimitecompra ]
  
  dtrain[ , mvr_mpagosdolares       := mv_mpagosdolares / mv_mlimitecompra ]
  dapply[ , mvr_mpagosdolares       := mv_mpagosdolares / mv_mlimitecompra ]
  
  dtrain[ , mvr_mconsumototal       := mv_mconsumototal  / mv_mlimitecompra ]
  dapply[ , mvr_mconsumototal       := mv_mconsumototal  / mv_mlimitecompra ]
  
  dtrain[ , mvr_mpagominimo         := mv_mpagominimo  / mv_mlimitecompra ]
  dapply[ , mvr_mpagominimo         := mv_mpagominimo  / mv_mlimitecompra ]

  #valvula de seguridad para evitar valores infinitos
  #paso los infinitos a NULOS
  infinitos_tr      <- lapply(names(dtrain),function(.name) dtrain[ , sum(is.infinite(get(.name)))])
  infinitos_qty_a  <- sum( unlist( infinitos_tr) )
  if( infinitos_qty_a > 0 )
  {
    cat( "ATENCION, hay", infinitos_qty_a, "valores infinitos en tu dataset de train. Seran pasados a NA\n" )
    dtrain[mapply(is.infinite, dtrain)] <- NA
  }
  
  
  infinitos_ap      <- lapply(names(dapply),function(.name) dapply[ , sum(is.infinite(get(.name)))])
  infinitos_qty_b  <- sum( unlist( infinitos_ap) )
  if( infinitos_qty_b > 0 )
  {
    cat( "ATENCION, hay", infinitos_qty_b, "valores infinitos en tu dataset. Seran pasados a NA\n" )
    dapply[mapply(is.infinite, dapply)] <- NA
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

  
  ### Agrego la parte robada ###
  
  
  
  
  
  
  
  #grabo con nombre extendido
  fwrite( dataset,
          file=arch_destino,
          sep= "," )
}
#------------------------------------------------------------------------------

dir.create( "./datasets/" )


#lectura rapida del dataset  usando fread  de la libreria  data.table
dataset1  <- fread("./datasets/competencia1_2022.csv")

EnriquecerDataset( dataset1, "./datasets/competencia1_2022_ext.csv" )

#quit( save="no")
