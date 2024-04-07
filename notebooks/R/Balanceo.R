# Carga de Librerias
library(FNN)
library(modeest)
library(dplyr)
library(parallel)

# Función para encontrar los k vecinos más cercanos
get_nn_neighbors <- function(df, target, minority.value, vars.numeric, scaled = FALSE,  k, seed){
  set.seed(seed)

  # Detección de las variables categóricas según la lista numérica
  vars.categoric <- names(df)[!names(df) %in% vars.numeric]
  vars.categoric <- vars.categoric[vars.categoric != target]

  # Escalamaiento de los Datos para el caso que sea requerido
  if (scaled){
    df[vars.numeric] <- lapply(df[vars.numeric], scale)
  }

  # Mutación de los conjuntos para asegurar que las variables sean factores y valores numéricos
  df <- df %>% mutate(across(all_of(vars.categoric), factor))
  #df = df %>% mutate(across(all_of(vars.numeric), numeric))


  # Ajuste del conjunto de la clase minoritaría
  minority.df <- df[df[[target]] == minority.value, ]

  # Creación de conjuntos Codificados para las variables categóricas
  encoded.categoric <- as.data.frame(sapply(df[vars.categoric], as.numeric))
  encoded.minority <- encoded.categoric[as.integer(rownames(minority.df)),]

  # Variables Necesarias para el cálculo de la distancia para SMOTE ENC
  t <- nrow(minority.df)
  s <- nrow(df)
  ir <- t/s
  c <- length(vars.numeric)
  m <- ifelse(c>0, median(sapply(minority.df[vars.numeric], sd)), 1)

  # Cálculo del Valor Asociado a cada nivel de cada variable categórica según SMOTE ENC
  l_values <- lapply(vars.categoric, function(var){
    labels <- unique(encoded.categoric[, var])
    e <- sapply(labels, function(l) sum(encoded.categoric[, var] == l))
    e_star <- e * ir
    o <- sapply(labels, function(l) sum(encoded.minority[, var] == l))
    chi <- (o - e_star)/(e_star)
    l <- chi*m
    l[labels]
  })
  names(l_values) <- vars.categoric

  # Creación del dataframe donde se almacenará la información numérica de los valores asociados a las variables categóricas
  encoded.categoric_updated <- encoded.categoric

  encoded.categoric_updated[vars.categoric] <- lapply(vars.categoric, function(var) {
  lapply(encoded.categoric[[var]], function(value) {
    l_values[[var]][value]
  })
})

  # Concatenación del resultado de las variables categóricas con las variable numéricas
  df_encoded <- cbind(df[vars.numeric], encoded.categoric_updated)
  df_encoded <- data.frame(df_encoded) 
  df_encoded_numeric <- mutate_all(df_encoded, as.numeric)

  # Detección de los k vecinos más cercanos
  knn <- get.knn(df_encoded_numeric, k)$nn.index

  return(knn)
}

# Función para hacer las muestras sintéticas de forma paralela
make_samples_parallel <- function(df, vars.numeric, target, minority.value, muestra, knn, seed, cl){
  # Plantación de Semilla
  set.seed(seed)

  # Detección de variables categóricas
  vars.categoric <- names(df)[!names(df) %in% vars.numeric]
  vars.categoric <- vars.categoric[vars.categoric != target]

  # Cargar la biblioteca `modeest` en los nodos
  clusterEvalQ(cl, library(modeest))

  # Exportar la función `mfv` a los nodos
  clusterExport(cl, "mfv")

  a <- parSapply(cl, muestra, function(id_muestra) {
    k_nn <- knn[id_muestra, ]
    nn <- sample(k_nn, 1)
    numeric.values <- sapply(vars.numeric, function(var){
      diff <- df[[var]][nn] - df[[var]][id_muestra]
      gap <- runif(1)
      value <- as.numeric(df[[var]][id_muestra] + gap*diff)
      return(value)
    })
    names(numeric.values) <- vars.numeric
  
    categoric.values <- sapply(vars.categoric, function(var) {
      value <- as.character(mfv(df[[var]][k_nn])[1])
      return(value)
    })
    names(categoric.values) <- vars.categoric
  
    synthetic <- c(numeric.values, categoric.values)
    return(synthetic)
  })
  
  result <- data.frame(t(a))
  result[[target]] = minority.value
  return(result)
}

# Función para hacer muestras sintéticas de forma normal
make_samples_no_parallel <- function(df, vars.numeric, target, minority.value, muestra, knn, seed){
  # Plantación de Semilla
  set.seed(seed)

  # Detección de variables categóricas
  vars.categoric <- names(df)[!names(df) %in% vars.numeric]
  vars.categoric <- vars.categoric[vars.categoric != target]

  a <- sapply(muestra, function(id_muestra) {
    k_nn <- knn[id_muestra, ]
    nn <- sample(k_nn, 1)
    numeric.values <- sapply(vars.numeric, function(var){
      diff <- df[[var]][nn] - df[[var]][id_muestra]
      gap <- runif(1)
      value <- as.numeric(df[[var]][id_muestra] + gap*diff)
      return(value)
    })
    names(numeric.values) <- vars.numeric
  
    categoric.values <- sapply(vars.categoric, function(var) {
      value <- as.character(mfv(df[[var]][k_nn])[1])
      return(value)
    })
    names(categoric.values) <- vars.categoric
  
    synthetic <- c(numeric.values, categoric.values)
    return(synthetic)
  })
  
  result <- data.frame(t(a))
  result[[target]] = minority.value
  return(result)
}

# Funcion SMOTE ENC
SMOTE_ENC <- function(df, target, minority.value, vars.numeric, k, parallel = FALSE, seed){
    
    ## Ajuste de los K vecinos más cercanos
    knn <- get_nn_neighbors(df, target, minority.value, vars.numeric, scaled = TRUE, k, seed)

    ## Establecer la muestra
    minor.df <- df[df[[target]] == minority.value, ]
    t <- nrow(minor.df)
    s <- nrow(df)

    N <- s - 2*t

    # Lista de indices de la clase minoritaría sobre la cuál se harán las muestras sintéticas
    index.minority <- as.integer(rownames(minor.df))
    
    if (N <= t){
        new_index <- sample(index.minority, N)
    } else {
        repeticiones <- floor(N/t)
        new_index <- rep(index.minority, repeticiones)
        restantes <- N - (repeticiones*t)
        new_index <- c(new_index, sample(index.minority, restantes))
    }
    # Ajuste de Espacio de Trabajo para el procesamiento en paralelo
    if (parallel){
      n_cores <- detectCores()
      cl <- makeCluster(n_cores)
      
      Synthetic <- make_samples_parallel(df, vars.numeric, target, minority.value, new_index, knn, seed, cl)
      stopCluster(cl)
      } else {
        Synthetic <- make_samples_no_parallel(df, vars.numeric, target, minority.value, new_index, knn, seed)
      }

    return(Synthetic)
}

ADASYN.ENC <- function(df, target, minority.value, vars.numeric, k, parallel = FALSE, seed){
    
    ## Ajuste de los K vecinos más cercanos
    knn <- get_nn_neighbors(df, target, minority.value, vars.numeric, scaled = FALSE, k, seed)

    ## Establecer la muestra
    minor.df <- df[df[[target]] == minority.value, ]
    mayor.df <- df[df[[target]]!= minority.value, ]

    G <- nrow(mayor.df) - nrow(minor.df)

    r_i <- sapply(as.numeric(rownames(minor.df)), function(i){
    value <- knn[i,] %in% as.numeric(rownames(mayor.df))
    r <- length(knn[i,value])/k
    return(r)
    })

    r_i_n <- r_i/sum(r_i)

    g_i <- data.frame(index = as.numeric(rownames(minor.df)), g = round(r_i_n*G,0))

    new_index <- unlist(lapply(1:nrow(g_i), function(i){
        index <- rep(g_i[i,'index'], g_i[i,'g'])
        return(index)
        }))

    # Ajuste de Espacio de Trabajo para el procesamiento en paralelo
    if (parallel){
      n_cores <- detectCores()
      cl <- makeCluster(n_cores)
      Synthetic <- make_samples_parallel(df, vars.numeric, target, minority.value, new_index, knn, seed, cl)
      
      stopCluster(cl)
      } else{
        Synthetic <- make_samples_no_parallel(df, vars.numeric, target, minority.value, new_index, knn, seed)
      }

      Synthetic <- Synthetic[sample(1:nrow(Synthetic), G), ]

    return(Synthetic)
}


WSMOTE.ENC  <- function(df, factor_expansion, target, minority.value, vars.numeric, k, parallel = FALSE,  seed){
    
    ## Ajuste de los K vecinos más cercanos
    knn <- get_nn_neighbors(df, target, minority.value, vars.numeric, scaled = TRUE, k, seed)

    ## Establecer la muestra
    minor.df <- df[df[[target]] == minority.value, ]
    mayor.df <- df[df[[target]]!= minority.value, ]

    G <- nrow(mayor.df) - nrow(minor.df)

    factor.expansion <- factor_expansion[as.numeric(rownames(minor.df))]
    r_i_n <- factor.expansion/sum(factor.expansion)

    g_i <- data.frame(index = as.numeric(rownames(minor.df)), g = round(r_i_n*G,0))

    new_index <- unlist(lapply(1:nrow(g_i), function(i){
        index <- rep(g_i[i,'index'], g_i[i,'g'])
        return(index)
        }))

    # Ajuste de Espacio de Trabajo para el procesamiento en paralelo
    if (parallel){
      n_cores <- detectCores()
      cl <- makeCluster(n_cores)
      Synthetic <- make_samples_parallel(df, vars.numeric, target, minority.value, new_index, knn, seed, cl)
      
      stopCluster(cl)
      } else{
        Synthetic <- make_samples_no_parallel(df, vars.numeric, target, minority.value, new_index, knn, seed)
      }

      Synthetic <- Synthetic[sample(1:nrow(Synthetic), G), ]

    return(Synthetic)
}