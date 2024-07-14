#!/usr/bin/env Rscript

# Workflow  Feature Engineering intrames manual artesanal

# inputs
#  * dataset
# output  
#   un dataset algo mas grande:
#     misma cantidad de registros
#     nuevos atributos construidos en forma artesanal y con mucho sufrimiento
#     generados en codigo R,  especificos para este dataset y clase

# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE) # garbage collection

require("data.table")
require("yaml")


#cargo la libreria
# args <- c( "~/labo2024r" )
args <- commandArgs(trailingOnly=TRUE)
source( paste0( args[1] , "/src/lib/action_lib.r" ) )

#------------------------------------------------------------------------------
# Esta es la parte que los alumnos deben desplegar todo su ingenio
# Agregar aqui sus PROPIAS VARIABLES manuales

AgregarVariables_IntraMes <- function(dataset) {
  cat( "inicio AgregarVariables_IntraMes()\n")
  gc()
  # INICIO de la seccion donde se deben hacer cambios con variables nuevas

  # el mes 1,2, ..12
  dataset[, kmes := foto_mes %% 100]

  # creo un ctr_quarter que tenga en cuenta cuando
  # los clientes hace 3 menos meses que estan
  # ya que seria injusto considerar las transacciones medidas en menor tiempo
  dataset[, ctrx_quarter_normalizado := as.numeric(ctrx_quarter) ]
  dataset[cliente_antiguedad == 1, ctrx_quarter_normalizado := ctrx_quarter * 5]
  dataset[cliente_antiguedad == 2, ctrx_quarter_normalizado := ctrx_quarter * 2]
  dataset[
    cliente_antiguedad == 3,
    ctrx_quarter_normalizado := ctrx_quarter * 1.2
  ]

  # variable extraida de una tesis de maestria de Irlanda
  dataset[, mpayroll_sobre_edad := mpayroll / cliente_edad]

  # se crean los nuevos campos para MasterCard  y Visa,
  #  teniendo en cuenta los NA's
  # varias formas de combinar Visa_status y Master_status
  dataset[, vm_status01 := pmax(Master_status, Visa_status, na.rm = TRUE)]
  dataset[, vm_status02 := Master_status + Visa_status]

  dataset[, vm_status03 := pmax(
    ifelse(is.na(Master_status), 10, Master_status),
    ifelse(is.na(Visa_status), 10, Visa_status)
  )]

  dataset[, vm_status04 := ifelse(is.na(Master_status), 10, Master_status)
    + ifelse(is.na(Visa_status), 10, Visa_status)]

  dataset[, vm_status05 := ifelse(is.na(Master_status), 10, Master_status)
    + 100 * ifelse(is.na(Visa_status), 10, Visa_status)]

  dataset[, vm_status06 := ifelse(is.na(Visa_status),
    ifelse(is.na(Master_status), 10, Master_status),
    Visa_status
  )]

  dataset[, mv_status07 := ifelse(is.na(Master_status),
    ifelse(is.na(Visa_status), 10, Visa_status),
    Master_status
  )]


  # combino MasterCard y Visa
  dataset[, vm_mfinanciacion_limite := rowSums(cbind(Master_mfinanciacion_limite, Visa_mfinanciacion_limite), na.rm = TRUE)]

  dataset[, vm_Fvencimiento := pmin(Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE)]
  dataset[, vm_Finiciomora := pmin(Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE)]
  dataset[, vm_msaldototal := rowSums(cbind(Master_msaldototal, Visa_msaldototal), na.rm = TRUE)]
  dataset[, vm_msaldopesos := rowSums(cbind(Master_msaldopesos, Visa_msaldopesos), na.rm = TRUE)]
  dataset[, vm_msaldodolares := rowSums(cbind(Master_msaldodolares, Visa_msaldodolares), na.rm = TRUE)]
  dataset[, vm_mconsumospesos := rowSums(cbind(Master_mconsumospesos, Visa_mconsumospesos), na.rm = TRUE)]
  dataset[, vm_mconsumosdolares := rowSums(cbind(Master_mconsumosdolares, Visa_mconsumosdolares), na.rm = TRUE)]
  dataset[, vm_mlimitecompra := rowSums(cbind(Master_mlimitecompra, Visa_mlimitecompra), na.rm = TRUE)]
  dataset[, vm_madelantopesos := rowSums(cbind(Master_madelantopesos, Visa_madelantopesos), na.rm = TRUE)]
  dataset[, vm_madelantodolares := rowSums(cbind(Master_madelantodolares, Visa_madelantodolares), na.rm = TRUE)]
  dataset[, vm_fultimo_cierre := pmax(Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE)]
  dataset[, vm_mpagado := rowSums(cbind(Master_mpagado, Visa_mpagado), na.rm = TRUE)]
  dataset[, vm_mpagospesos := rowSums(cbind(Master_mpagospesos, Visa_mpagospesos), na.rm = TRUE)]
  dataset[, vm_mpagosdolares := rowSums(cbind(Master_mpagosdolares, Visa_mpagosdolares), na.rm = TRUE)]
  dataset[, vm_fechaalta := pmax(Master_fechaalta, Visa_fechaalta, na.rm = TRUE)]
  dataset[, vm_mconsumototal := rowSums(cbind(Master_mconsumototal, Visa_mconsumototal), na.rm = TRUE)]
  dataset[, vm_cconsumos := rowSums(cbind(Master_cconsumos, Visa_cconsumos), na.rm = TRUE)]
  dataset[, vm_cadelantosefectivo := rowSums(cbind(Master_cadelantosefectivo, Visa_cadelantosefectivo), na.rm = TRUE)]
  dataset[, vm_mpagominimo := rowSums(cbind(Master_mpagominimo, Visa_mpagominimo), na.rm = TRUE)]

  # a partir de aqui juego con la suma de Mastercard y Visa
  dataset[, vmr_Master_mlimitecompra := Master_mlimitecompra / vm_mlimitecompra]
  dataset[, vmr_Visa_mlimitecompra := Visa_mlimitecompra / vm_mlimitecompra]
  dataset[, vmr_msaldototal := vm_msaldototal / vm_mlimitecompra]
  dataset[, vmr_msaldopesos := vm_msaldopesos / vm_mlimitecompra]
  dataset[, vmr_msaldopesos2 := vm_msaldopesos / vm_msaldototal]
  dataset[, vmr_msaldodolares := vm_msaldodolares / vm_mlimitecompra]
  dataset[, vmr_msaldodolares2 := vm_msaldodolares / vm_msaldototal]
  dataset[, vmr_mconsumospesos := vm_mconsumospesos / vm_mlimitecompra]
  dataset[, vmr_mconsumosdolares := vm_mconsumosdolares / vm_mlimitecompra]
  dataset[, vmr_madelantopesos := vm_madelantopesos / vm_mlimitecompra]
  dataset[, vmr_madelantodolares := vm_madelantodolares / vm_mlimitecompra]
  dataset[, vmr_mpagado := vm_mpagado / vm_mlimitecompra]
  dataset[, vmr_mpagospesos := vm_mpagospesos / vm_mlimitecompra]
  dataset[, vmr_mpagosdolares := vm_mpagosdolares / vm_mlimitecompra]
  dataset[, vmr_mconsumototal := vm_mconsumototal / vm_mlimitecompra]
  dataset[, vmr_mpagominimo := vm_mpagominimo / vm_mlimitecompra]

  # Aqui debe usted agregar sus propias nuevas variables
  
  #normalizo los montos de clientes nuevos
  # mcuenta_corriente_adicional
  dataset[, mcuenta_corriente_adicional_normalizado := as.numeric(mcuenta_corriente_adicional) ]
  dataset[cliente_antiguedad == 1, mcuenta_corriente_adicional_normalizado := mcuenta_corriente_adicional * 2]
  # mcuenta_corriente
  dataset[, mcuenta_corriente_normalizado := as.numeric(mcuenta_corriente) ]
  dataset[cliente_antiguedad == 1, mcuenta_corriente_normalizado := mcuenta_corriente * 2]
  # mcaja_ahorro
  dataset[, mcaja_ahorro_normalizado := as.numeric(mcaja_ahorro) ]
  dataset[cliente_antiguedad == 1, mcaja_ahorro_normalizado := mcaja_ahorro * 2]
  # mcaja_ahorro_adicional
  dataset[, mcaja_ahorro_adicional_normalizado := as.numeric(mcaja_ahorro_adicional) ]
  dataset[cliente_antiguedad == 1, mcaja_ahorro_adicional_normalizado := mcaja_ahorro_adicional * 2]
  # mcaja_ahorro_dolares
  dataset[, mcaja_ahorro_dolares_normalizado := as.numeric(mcaja_ahorro_dolares) ]
  dataset[cliente_antiguedad == 1, mcaja_ahorro_dolares_normalizado := mcaja_ahorro_dolares * 2]
  # mcuentas_saldo
  dataset[, mcuentas_saldo_normalizado := as.numeric(mcuentas_saldo) ]
  dataset[cliente_antiguedad == 1, mcuentas_saldo_normalizado := mcuentas_saldo * 2]
  # mautoservicio
  dataset[, mcuentas_saldo_normalizado := as.numeric(mcuentas_saldo) ]
  dataset[cliente_antiguedad == 1, mcuentas_saldo_normalizado := mcuentas_saldo * 2]
  
  #normalizo los montos ganancia del banco por clientes nuevos
  # mrentabilidad
  dataset[, mrentabilidad_normalizado := as.numeric(mrentabilidad) ]
  dataset[cliente_antiguedad == 1, mrentabilidad_normalizado := mrentabilidad * 2]
  # mrentabilidad_annual
  dataset[, mrentabilidad_annual_normalizado := as.numeric(mrentabilidad_annual) ]
  dataset[cliente_antiguedad == 1, mrentabilidad_annual_normalizado := mrentabilidad_annual * 2]
  # mcomisiones
  dataset[, mcomisiones_normalizado := as.numeric(mcomisiones) ]
  dataset[cliente_antiguedad == 1, mcomisiones_normalizado := mcomisiones * 2]
  # mactivos_margen
  dataset[, mactivos_margen_normalizado := as.numeric(mactivos_margen) ]
  dataset[cliente_antiguedad == 1, mactivos_margen_normalizado := mactivos_margen * 2]
  # mpasivos_margen
  dataset[, mpasivos_margen_normalizado := as.numeric(mpasivos_margen) ]
  dataset[cliente_antiguedad == 1, mpasivos_margen_normalizado := mpasivos_margen * 2]
  

  # la cuenta tiene saldo cero en el mes
  dataset[, tiene_saldo_cero_fe := ifelse(mcuentas_saldo == 0, 1, 0)]
  
  # salario estimado -> usa los limites de credito como base de la eestimacion vm_mlimitecompra creada en baseline
  dataset[, salario_estimado_fe := as.numeric(mpayroll) ]
  dataset[, salario_estimado_fe := ifelse(is.na(mpayroll) & is.na(mpayroll2), (vm_mlimitecompra) / 3, mpayroll + mpayroll2)]
  
  #activo
  dataset[, mactivo_fe := rowSums(cbind(mcuentas_saldo, mplazo_fijo_dolares, mplazo_fijo_pesos, minversion1_pesos, minversion1_dolares, minversion2), na.rm = TRUE)]
  #pasivo
  dataset[, mpasivo_fe := rowSums(cbind(mprestamos_personales, mprestamos_prendarios, mprestamos_hipotecarios,Visa_msaldopesos, Master_msaldototal), na.rm = TRUE)]
  #patrimonio neto
  dataset[, mpatrimonio_neto_fe := rowSums(cbind(mactivo_fe , (-1) * mpasivo_fe), na.rm = TRUE)]
  #apalancamiento o leverage financiero
  dataset[, r_apalancamiento_fe :=  mpasivo_fe/mactivo_fe ]
  
  #inversiones realizadas en el banco
  dataset[, mtotal_inversiones_fe := rowSums(cbind(mplazo_fijo_dolares, mplazo_fijo_pesos, minversion1_pesos, minversion1_dolares, minversion2), na.rm = TRUE)]
  dataset[, ctotal_inversiones_fe := rowSums(cbind(cplazo_fijo+ cinversion1 + cinversion2), na.rm = TRUE)]
  #inversion promedio
  dataset[, mprom_inversion_fe :=  mtotal_inversiones_fe/ctotal_inversiones_fe ]
  
  #prestamos tomados con el banco
  dataset[, mtotal_prestamos_fe := rowSums(cbind(mprestamos_personales, mprestamos_prendarios, mprestamos_hipotecarios), na.rm = TRUE)]
  dataset[, ctotal_prestamos_fe := rowSums(cbind(cprestamos_personales, cprestamos_prendarios,	cprestamos_hipotecarios), na.rm = TRUE)]
  # prestamo promedio
  dataset[, mprom_prestamo_fe :=  mtotal_prestamos_fe/ctotal_prestamos_fe]
  
  #cantidad total de seguros contratados con el banco
  dataset[, ctotal_seguros_fe := rowSums(cbind(cseguro_vida +cseguro_auto+	cseguro_vivienda + cseguro_accidentes_personales), na.rm = TRUE)]
  
  
  # ratio saldo vs salario estimado
  dataset[, r_saldo_vs_salario_fe :=  mcuentas_saldo/salario_estimado_fe ]
  
  # Ratio de Antigüedad del Client
  dataset[, r_antig_vs_edad_fe :=  cliente_antiguedad /cliente_edad ]
  
  #salario estimado vs edad
  dataset[, r_salario_estimado_vs_edad_fe :=  salario_estimado_fe /cliente_edad ]
  
  #inveriones vs edad
  dataset[, r_inversiones_vs_edad_fe :=  mtotal_inversiones_fe /cliente_edad ]
  
  #activo vs edad
  dataset[, r_activo_vs_edad_fe :=  mactivo_fe /cliente_edad]
  
  #prestamos vs edad
  dataset[, r_prestamos_vs_edad_fe :=  mtotal_prestamos_fe /cliente_edad]
  
  #pasivoo vs edad
  dataset[, r_pasivo_vs_edad_fe :=  mpasivo_fe /cliente_edad]
  
  #tarjetas de credito: 
  #libre para consumir: vm_mlimitecompra - vm_msaldototal
  #dataset[, vm_mlibre_consumir_fe := rowSums(cbind(vm_mlimitecompra , (-1) * vm_msaldototal), na.rm = TRUE)]
  dataset[, vm_mlibre_consumir_fe :=  vm_mlimitecompra - vm_msaldototal ]
  
  #Consumo del mes en relacion al saldo de la tarjeta --> comparar consumo actual versus adeudado
  dataset[, r_vm_consumoactual_vs_deuda_fe := vm_mconsumototal / vm_msaldototal ]
  
  
  # valvula de seguridad para evitar valores infinitos
  # paso los infinitos a NULOS
  infinitos <- lapply(
    names(dataset),
    function(.name) dataset[, sum(is.infinite(get(.name)))]
  )

  infinitos_qty <- sum(unlist(infinitos))
  if (infinitos_qty > 0) {
    cat(
      "ATENCION, hay", infinitos_qty,
      "valores infinitos en tu dataset. Seran pasados a NA\n"
    )
    dataset[mapply(is.infinite, dataset)] <<- NA
  }


  # valvula de seguridad para evitar valores NaN  que es 0/0
  # paso los NaN a 0 , decision polemica si las hay
  # se invita a asignar un valor razonable segun la semantica del campo creado
  nans <- lapply(
    names(dataset),
    function(.name) dataset[, sum(is.nan(get(.name)))]
  )

  nans_qty <- sum(unlist(nans))
  if (nans_qty > 0) {
    cat(
      "ATENCION, hay", nans_qty,
      "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n"
    )

    cat("Si no te gusta la decision, modifica a gusto el programa!\n\n")
    dataset[mapply(is.nan, dataset)] <<- 0
  }

  cat( "fin AgregarVariables_IntraMes()\n")
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa
cat( "z1301_FE_intrames_manual.r  START\n")
action_inicializar() 


# cargo el dataset donde voy a entrenar
# esta en la carpeta del exp_input y siempre se llama  dataset.csv.gz
# cargo el dataset
envg$PARAM$dataset <- paste0( "./", envg$PARAM$input, "/dataset.csv.gz" )
envg$PARAM$dataset_metadata <- read_yaml( paste0( "./", envg$PARAM$input, "/dataset_metadata.yml" ) )

cat( "lectura del dataset\n")
action_verificar_archivo( envg$PARAM$dataset )
cat( "Iniciando lectura del dataset\n" )
dataset <- fread(envg$PARAM$dataset)
cat( "Finalizada lectura del dataset\n" )

GrabarOutput()

# Agrego las variables manuales
cat( "variables intra mest\n")
AgregarVariables_IntraMes(dataset)

#------------------------------------------------------------------------------
# grabo el dataset
cat( "grabado del dataset\n")
cat( "Iniciando grabado del dataset\n" )
fwrite(dataset,
  file = "dataset.csv.gz",
  logical01 = TRUE,
  sep = ","
)
cat( "Finalizado grabado del dataset\n" )


# copia la metadata sin modificar
cat( "grabado de metadata\n")
write_yaml( envg$PARAM$dataset_metadata, 
  file="dataset_metadata.yml" )

#------------------------------------------------------------------------------

# guardo los campos que tiene el dataset
tb_campos <- as.data.table(list(
  "pos" = 1:ncol(dataset),
  "campo" = names(sapply(dataset, class)),
  "tipo" = sapply(dataset, class),
  "nulos" = sapply(dataset, function(x) {
    sum(is.na(x))
  }),
  "ceros" = sapply(dataset, function(x) {
    sum(x == 0, na.rm = TRUE)
  })
))

fwrite(tb_campos,
  file = "dataset.campos.txt",
  sep = "\t"
)

#------------------------------------------------------------------------------
cat( "Fin del programa\n")

envg$OUTPUT$dataset$ncol <- ncol(dataset)
envg$OUTPUT$dataset$nrow <- nrow(dataset)
envg$OUTPUT$time$end <- format(Sys.time(), "%Y%m%d %H%M%S")
GrabarOutput()

#------------------------------------------------------------------------------
# finalizo la corrida
#  archivos tiene a los files que debo verificar existen para no abortar

action_finalizar( archivos = c("dataset.csv.gz","dataset_metadata.yml")) 
cat( "z1301_FE_intrames_manual.r  END\n")
