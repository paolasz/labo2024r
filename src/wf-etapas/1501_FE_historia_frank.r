#!/usr/bin/env Rscript

# Workflow  Feature Engineering historico

# inputs
#  * gran dataset
#  * especificaciones de nuevos atributos historicos
# output  
#   muuuy gran dataset :
#     misma cantidad de registros
#     los valores de los campos no se modifican
#     agregado de nuevos atributos, basados en la historia


# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE) # garbage collection

require("data.table")
require("yaml")
require("Rcpp")

#cargo la libreria
# args <- c( "~/labo2024r" )
args <- commandArgs(trailingOnly=TRUE)
source( paste0( args[1] , "/src/lib/action_lib.r" ) )

#------------------------------------------------------------------------------
# se calculan para los x meses previos el minimo, maximo y
#  tendencia calculada con cuadrados minimos
# la formula de calculo de la tendencia puede verse en
#  https://stats.libretexts.org/Bookshelves/Introductory_Statistics/Book%3A_Introductory_Statistics_(Shafer_and_Zhang)/10%3A_Correlation_and_Regression/10.04%3A_The_Least_Squares_Regression_Line
# para la maxíma velocidad esta funcion esta escrita en lenguaje C,
# y no en la porqueria de R o Python

cppFunction("NumericVector fhistC(
            NumericVector pcolumna,
            IntegerVector pdesde, 
            int ventana,
            double alpha)
{
  /* Aqui se cargan los valores para la regresion */
  double  x[100] ;
  double  y[100] ;

  int n = pcolumna.size();
  NumericVector out( 7*n );

  for(int i = 0; i < n; i++)
  {
    //lag
    if( pdesde[i]-1 < i )  out[ i + 4*n ]  =  pcolumna[i-1] ;
    else                   out[ i + 4*n ]  =  NA_REAL ;

    

    int  libre    = 0 ;
    int  xvalor   = 1 ;

    for( int j= pdesde[i]-1;  j<=i; j++ )
    {
       double a = pcolumna[j] ;
       

       if( !R_IsNA( a ) )
       {
          y[ libre ]= a ;
          x[ libre ]= xvalor ;
          libre++ ;
       }

       xvalor++ ;
    }

    
    /* Si hay al menos dos valores */
    if( libre > 1 )
    {
      double  xsum  = x[0] ;
      double  ysum  = y[0] ;
      double  xysum = xsum * ysum ;
      double  xxsum = xsum * xsum ;
      double  vmin  = y[0] ;
      double  vmax  = y[0] ;
      double  current_val = pcolumna[i] ;

      // Defino valores para EMA cuando libre < ventana y
      // el primer valor de EMA donde se usa SMA como aproximacion
      // de la EMA del periodo anterior.

      if( libre <= ventana-1)
      {
        out[ i + 5*n ]  = NA_REAL ; // EMA  
      } else if(libre == ventana && R_IsNA( out[ i + 5*n -1] ) )
      { // primer valor de EMA
        out[ i + 5*n ] = current_val*alpha + out[ i + 3*n -1]*(1-alpha);
      } else
      { // actualizo EMA
        out[ i + 5*n ] = current_val*alpha + out[ i + 5*n -1]*(1-alpha);
      }

      
      // Calculo otras tendencias:
      
      for( int h=1; h<libre; h++)
      {
        xsum  += x[h] ;
        ysum  += y[h] ;
        xysum += x[h]*y[h] ;
        xxsum += x[h]*x[h] ;

        if( y[h] < vmin )  vmin = y[h] ;
        if( y[h] > vmax )  vmax = y[h] ;


      }

      out[ i ]  =  (libre*xysum - xsum*ysum)/(libre*xxsum -xsum*xsum);
      out[ i + n ]    =  vmin ;
      out[ i + 2*n ]  =  vmax ;
      out[ i + 3*n ]  =  ysum / libre ; // media movil 
      out[ i + 6*n ]  = sqrt(pow(out[ i + 3*n ] - current_val,2) / libre );

      
    }
    else
    {
      out[ i       ]  =  NA_REAL ;
      out[ i + n   ]  =  NA_REAL ;
      out[ i + 2*n ]  =  NA_REAL ;
      out[ i + 3*n ]  =  NA_REAL ; 
      out[ i + 5*n ]  =  NA_REAL ; // EMA
      out[ i + 6*n ]  =  NA_REAL ; // BBW
    }
  }

  return  out;
}")






cppFunction("NumericVector fBBWP(
            NumericVector pcolumna,
            IntegerVector pdesde
            )
            
{ /* Aqui se cargan los valores para realizar las operaciones */
  /* Utilizo un set dado que no guarda duplicados, y ordena los datos */
  std::set<double> y ;

  int n = pcolumna.size();
  NumericVector out( n );
  
  
  
  
  for(int i = 0; i < n; i++)
  {
    //Rcpp::Rcout << \"Iteration i: \" << i << std::endl;

    int  libre    = 0 ;
  
    for( int j= pdesde[i]-1;  j<=i; j++ )
    {
       double a = pcolumna[j] ;
       
       //Rcpp::Rcout << \"  Inner loop j: \" << j << \", a: \" << a << std::endl;

       if( !R_IsNA( a ) )
       {
          y.insert(a) ;
          libre++ ;
       }
       
    }
    
         
    /* Si hay al menos 3 valores */
    
    if( libre > 2 )
    {
      double k = pcolumna[i];
      
      /* Busco la posicion del valor BBW correspondiente a i */
      auto it = std::lower_bound(y.begin(), y.end(), k);
      double cantidad_bbw_menores = std::distance(y.begin(), it);
      
      /* La posicion dentro de y ordenado, es equivalente a la cantidad de 
         valores en y, menores que k. Es decir, obtengo cuantos valores de BBW en
         la ventana son menores BBW en i */
      
      out[i] = cantidad_bbw_menores/libre ;
      
    } else {
      
      out[i] = NA_REAL ;
    
    }
      
      
     /* Vaciamos y para la proxima iteracion */
     y.clear();
    
    }
    
    return out; 
}")

#------------------------------------------------------------------------------
# calcula la tendencia de las variables cols de los ultimos 6 meses
# la tendencia es la pendiente de la recta que ajusta por cuadrados minimos
# La funcionalidad de ratioavg es autoria de  Daiana Sparta,  UAustral  2021

TendenciaYmuchomas <- function(
    dataset, cols, ventana = 6, tendencia = TRUE,
    minimo = TRUE, maximo = TRUE, promedio = TRUE,
    ratioavg = FALSE, ratiomax = FALSE, ema = TRUE, bbwp = FALSE,
    bbwp_ventana = 3) {
  
  gc()
  
  # Esta es la cantidad de meses que utilizo para la historia
  ventana_regresion <- ventana

  # Verifico parametros
  cat("BBWP", bbwp)
  cat("bbwp ventana", bbwp_ventana)

  # Coeficiente de suavizado EMA: cuanto mas chico mejor suaviza, pero 
  # mas atrasa la tendencia
  alpha <- 2 / (1 + ventana)
  
  last <- nrow(dataset)
  
  # creo el vector_desde que indica cada ventana
  # de esta forma se acelera el procesamiento ya que lo hago una sola vez
  vector_ids <- dataset[ , get( envg$PARAM$dataset_metadata$entity_id) ]
  
  vector_desde <- seq(
    -ventana_regresion + 2,
    nrow(dataset) - ventana_regresion + 1
  )
  
  vector_desde[1:ventana_regresion] <- 1
  
  for (i in 2:last) {
    if (vector_ids[i - 1] != vector_ids[i]) {
      vector_desde[i] <- i
    }
  }
  for (i in 2:last) {
    if (vector_desde[i] < vector_desde[i - 1]) {
      vector_desde[i] <- vector_desde[i - 1]
    }
  }
  
  # Si BBWP es TRUE, calculo un nuevo vector desde con su ventana correspondiente
  if(bbwp){
    
    vector_desde_bbwp <- seq(
      -bbwp_ventana + 2,
      nrow(dataset) - bbwp_ventana + 1
    )
    
    vector_desde_bbwp[1:bbwp_ventana] <- 1
    
    for (i in 2:last) {
      if (vector_ids[i - 1] != vector_ids[i]) {
        vector_desde_bbwp[i] <- i
      }
    }
    
    for (i in 2:last) {
      if (vector_desde_bbwp[i] < vector_desde_bbwp[i - 1]) {
        vector_desde_bbwp[i] <- vector_desde_bbwp[i - 1]
      }
    }
  }
  
  for (campo in cols) {
    nueva_col <- fhistC(dataset[, get(campo)], vector_desde, ventana, alpha)

    if (tendencia) {
      dataset[, paste0(campo, "_tend", ventana) :=
                nueva_col[(0 * last + 1):(1 * last)]]
    } 
    
    if (minimo) {
      dataset[, paste0(campo, "_min", ventana) :=
                nueva_col[(1 * last + 1):(2 * last)]]
    }
    
    if (maximo) {
      dataset[, paste0(campo, "_max", ventana) :=
                nueva_col[(2 * last + 1):(3 * last)]]
    }
    
    if (promedio) {
      dataset[, paste0(campo, "_avg", ventana) :=
                nueva_col[(3 * last + 1):(4 * last)]]
    }
    
    if (ratioavg) {
      dataset[, paste0(campo, "_ratioavg", ventana) :=
                get(campo) / nueva_col[(3 * last + 1):(4 * last)]]
    }
    
    if (ratiomax) {
      dataset[, paste0(campo, "_ratiomax", ventana) :=
                get(campo) / nueva_col[(2 * last + 1):(3 * last)]]
    }

    if (ema) {
      dataset[, paste0(campo, "_ema", ventana) :=
                nueva_col[(5 * last + 1):(6 * last)]]
    }
    
    
    if (bbwp) {
      bbw = nueva_col[(6 * last + 1):(7 * last)]
      
      # Calculo los BBWP
      bbwp_col = fBBWP(bbw, vector_desde_bbwp)
      
      # Agrego las columnas BBWP
      dataset[, paste0(campo, "_bbwp", bbwp_ventana) :=
                bbwp_col
                ]
                
    }
    

  }
}


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
cat( "z1501_FE_historia.r  START\n")
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


colnames(dataset)[which(!(sapply(dataset, typeof) %in% c("integer", "double")))]


GrabarOutput()

#--------------------------------------
# estas son las columnas a las que se puede agregar
#  lags o media moviles ( todas menos las obvias )

campitos <- c( envg$PARAM$dataset_metadata$primarykey,
  envg$PARAM$dataset_metadata$entity_id,
  envg$PARAM$dataset_metadata$periodo,
  envg$PARAM$dataset_metadata$clase )

campitos <- unique( campitos )


cols_lagueables <- copy(setdiff(
  colnames(dataset),
  envg$PARAM$dataset_metadata
))

# ordeno el dataset por primary key
#  es MUY  importante esta linea
# ordeno dataset
setorderv(dataset, envg$PARAM$dataset_metadata$primarykey)

# Lags variables bases:

if (envg$PARAM$lag1) {
  cat( "Inicio lag1\n")
  # creo los campos lags de orden 1
  envg$OUTPUT$lag1$ncol_antes <- ncol(dataset)
  dataset[, paste0(cols_lagueables, "_lag1") := shift(.SD, 1, NA, "lag"),
    by = eval( envg$PARAM$dataset_metadata$entity_id),
    .SDcols = cols_lagueables
  ]

  # agrego los delta lags de orden 1
  for (vcol in cols_lagueables)
  {
    dataset[, paste0(vcol, "_delta1") := get(vcol) - get(paste0(vcol, "_lag1"))]
  }

  # Elimino las columnas de los lags, solo dejo los delta lags
  dataset[, paste0(cols_lagueables, "_lag1") := NULL]
 

  envg$OUTPUT$lag1$ncol_despues <- ncol(dataset)
  GrabarOutput()
  cat( "Fin lag1\n")
}


cols_lagueables <- intersect(cols_lagueables, colnames(dataset))
if (envg$PARAM$lag2) {
  cat( "Inicio lag2\n")
  # creo los campos lags de orden 2
  envg$OUTPUT$lag2$ncol_antes <- ncol(dataset)
  dataset[, paste0(cols_lagueables, "_lag2") := shift(.SD, 2, NA, "lag"),
    by = eval(envg$PARAM$dataset_metadata$entity_id), 
    .SDcols = cols_lagueables
  ]

  # agrego los delta lags de orden 2
  for (vcol in cols_lagueables)
  {
    dataset[, paste0(vcol, "_delta2") := get(vcol) - get(paste0(vcol, "_lag2"))]
  }

  # Elimino las columnas de los lags, solo dejo los delta lags
  dataset[, paste0(cols_lagueables, "_lag2") := NULL]

  envg$OUTPUT$lag2$ncol_despues <- ncol(dataset)
  GrabarOutput()
  cat( "Fin lag2\n")
}


cols_lagueables <- intersect(cols_lagueables, colnames(dataset))
if (envg$PARAM$lag3) {
  cat( "Inicio lag3\n")
  # creo los campos lags de orden 3
  envg$OUTPUT$lag3$ncol_antes <- ncol(dataset)
  dataset[, paste0(cols_lagueables, "_lag3") := shift(.SD, 3, NA, "lag"),
    by = eval(envg$PARAM$dataset_metadata$entity_id),
    .SDcols = cols_lagueables
  ]

  # agrego los delta lags de orden 3
  for (vcol in cols_lagueables)
  {
    dataset[, paste0(vcol, "_delta3") := get(vcol) - get(paste0(vcol, "_lag3"))]
  }

  # Elimino las columnas de los lags, solo dejo los delta lags
  dataset[, paste0(cols_lagueables, "_lag3") := NULL]
  
  envg$OUTPUT$lag3$ncol_despues <- ncol(dataset)
  GrabarOutput()
  cat( "Fin lag3\n")
}


#--------------------------------------
# agrego las tendencias

# ordeno el dataset por primary key
#  es MUY  importante esta linea
cat( "ordenado dataset\n")
setorderv(dataset, envg$PARAM$dataset_metadata$primarykey)

# Verifico parametros globales
cat("Ema global: ", envg$PARAM$Tendencias1$ema)
cat("bbwp global:", envg$PARAM$Tendencias1$bbwp)
cat("bbwp ventana global:", envg$PARAM$Tendencias1$bbwp_ventana)

cols_lagueables <- intersect(cols_lagueables, colnames(dataset))
if (envg$PARAM$Tendencias1$run) {
  envg$OUTPUT$TendenciasYmuchomas1$ncol_antes <- ncol(dataset)
  TendenciaYmuchomas(dataset,
    cols = cols_lagueables,
    ventana = envg$PARAM$Tendencias1$ventana, 
    tendencia = envg$PARAM$Tendencias1$tendencia,
    minimo = envg$PARAM$Tendencias1$minimo,
    maximo = envg$PARAM$Tendencias1$maximo,
    promedio = envg$PARAM$Tendencias1$promedio,
    ratioavg = envg$PARAM$Tendencias1$ratioavg,
    ratiomax = envg$PARAM$Tendencias1$ratiomax,
    ema = envg$PARAM$Tendencias1$ema,
    bbwp = envg$PARAM$Tendencias1$bbwp,
    bbwp_ventana = envg$PARAM$Tendencias1$bbwp_ventana
  )

  envg$OUTPUT$TendenciasYmuchomas1$ncol_despues <- ncol(dataset)
  GrabarOutput()
}

# Introduzco delta-lags 2do orden para tendencias seleccionadas. 

if (envg$PARAM$delta_lags2_ema) {
  cat( "Inicio delta lags 2do orden para ema\n")

  # Selecciono columnas de tendencias:
  patron <- "_ema"
  tend_cols <- grep(patron, names(dataset), value = TRUE)

  # creo los campos lags 
  envg$OUTPUT$delta2tend$ncol_antes <- ncol(dataset)
  dataset[, paste0(tend_cols, "_lag2") := shift(.SD, 2, NA, "lag"),
    by = eval(envg$PARAM$dataset_metadata$entity_id),
    .SDcols = tend_cols
  ]

  # agrego los delta lags de orden 2
  for (vcol in tend_cols)
  {
    dataset[, paste0(vcol, "_delta2") := get(vcol) - get(paste0(vcol, "_lag2"))]
  }

  # Elimino las columnas de los lags, solo dejo los delta lags
  dataset[, paste0(tend_cols, "_lag2") := NULL]
  
  envg$OUTPUT$delta2tend$ncol_despues <- ncol(dataset)
  GrabarOutput()
  cat( "Fin delta lags 2do orden - para ema\n")
}


# Tendencias 2do orden

cols_lagueables <- intersect(cols_lagueables, colnames(dataset))
if (envg$PARAM$Tendencias2$run) {
  envg$OUTPUT$TendenciasYmuchomas2$ncol_antes <- ncol(dataset)
  TendenciaYmuchomas(dataset,
    cols = cols_lagueables,
    ventana = envg$PARAM$Tendencias2$ventana, # 6 meses de historia
    tendencia = envg$PARAM$Tendencias2$tendencia,
    minimo = envg$PARAM$Tendencias2$minimo,
    maximo = envg$PARAM$Tendencias2$maximo,
    promedio = envg$PARAM$Tendencias2$promedio,
    ratioavg = envg$PARAM$Tendencias2$ratioavg,
    ratiomax = envg$PARAM$Tendencias2$ratiomax,
    ema = envg$PARAM$Tendencias2$ema,
    bbwp = envg$PARAM$Tendencias2$bbwp,
    bbwp_ventana = envg$PARAM$Tendencias2$bbwp_ventana
  )

  envg$OUTPUT$TendenciasYmuchomas2$ncol_despues <- ncol(dataset)
  GrabarOutput()
}



#------------------------------------------------------------------------------
# grabo el dataset
cat( "grabado dataset\n")
cat( "Iniciando grabado del dataset\n" )
fwrite(dataset,
  file = "dataset.csv.gz",
  logical01 = TRUE,
  sep = ","
)
cat( "Finalizado grabado del dataset\n" )

# copia la metadata sin modificar
cat( "grabado metadata\n")
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
cat( "z1501_FE_historia.r  END\n")
