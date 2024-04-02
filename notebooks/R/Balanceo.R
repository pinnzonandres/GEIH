# Carga de Librerias
library(FNN)
library(modeest)
library(dplyr)
library(parallel)

# Código Fuente con el método de generación de muestras de FSDS
source("C:\\Users\\andre\\OneDrive\\Escritorio\\Proyecto de Grado\\notebooks\\R\\FSDS.R")


# Función para encontrar los k vecinos más cercanos según la métrica dada por SMOTE.ENC
SMOTE.ENC.KNN <- function(df, target, minority.value, vars.numeric, scaled = FALSE,  k, seed){
  set.seed(seed)

  # Detección de las variables categóricas según la lista numérica
  vars.categoric <- names(df)[!names(df) %in% vars.numeric]
  vars.categoric <- vars.categoric[vars.categoric != target]

  # Escalamaiento de los Datos para el caso que sea requerido
  if (scaled){
    df[vars.numeric] <- lapply(df[vars.numeric], scale)
  }

  # Mutación de los conjuntos para asegurar que las variables sean factores y valores numéricos
  df = df %>% mutate(across(all_of(vars.categoric), factor))
  #df = df %>% mutate(across(all_of(vars.numeric), numeric))


  # Ajuste del conjunto de la clase minoritaría
  minority.df = df[df[[target]] == minority.value, ]

  # Creación de conjuntos Codificados para las variables categóricas
  encoded.categoric = as.data.frame(sapply(df[vars.categoric], as.numeric))
  encoded.minority = encoded.categoric[as.integer(rownames(minority.df)),]

  # Variables Necesarias para el cálculo de la distancia para SMOTE ENC
  t = nrow(minority.df)
  s = nrow(df)
  ir = t/s
  c = length(vars.numeric)
  m = ifelse(c>0, median(sapply(minority.df[vars.numeric], sd)), 1)

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
  df_encoded = data.frame(df_encoded) 
  df_encoded_numeric <- mutate_all(df_encoded, as.numeric)

  # Detección de los k vecinos más cercanos
  knn = get.knn(df_encoded_numeric, k)$nn.index

  return(knn)
}


SyntheticData <- function(df, vars.numeric, target, minority.value, minority.less, muestra, knn, str_weight_name, seed, cl){
  # Plantación de Semilla
  set.seed(seed)

  # Detección de variables categóricas

  vars.categoric <- names(df)[!names(df) %in% vars.numeric]
  vars.categoric <- vars.categoric[vars.categoric != target]

  # Cargar la biblioteca `modeest` en los nodos
  clusterEvalQ(cl, library(modeest))

  # Exportar la función `mfv` a los nodos
  clusterExport(cl, "mfv")

  a <- parSapply(cl, 1:minority.less, function(j) {
    id_muestra <- muestra[j, "indice"]
    k_nn <- knn[id_muestra, ]
    nn <- sample(k_nn, 1)
    numeric.values <- sapply(vars.numeric, function(var){
      diff <- df[[var]][nn] - df[[var]][id_muestra]
      gap <- runif(1)
      return(df[[var]][id_muestra] + gap*diff)
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
  
  result = data.frame(t(a))
  result[[str_weight_name]] = muestra[, 'weight']
  result[[target]] = minority.value
  return(result)
}

SMOTE.ENC.FSDS <- function(df, target, minority.value, str_weight_name, k, vars.numeric, seed) {
  # Plantación de Semilla
  set.seed(seed)
  vars.numeric <- vars.numeric[vars.numeric != str_weight_name]
  
  # Detección del conjunto minoritario
  minority.df = df[df[[target]] == minority.value, ]
  
  # Definición de valores necesarios
  S = nrow(df)
  minority.less = S - 2*nrow(minority.df)
  
  m = 1
  n = S + minority.less
  N = round(sum(df[[str_weight_name]]),0)
  
  # Ajuste de Pesos
  factor.expansion <- df[['FACTOR_EXPANSION']]
  # Ajuste de Espacio de Trabajo para el procesamiento en paralelo
  n_cores <- detectCores()
  cl <- makeCluster(n_cores)

  # Generación de Muestra
  print("Making Muestra")
  D_m <- FSDS(minority.df, factor.expansion, N, m, n, seed, cl)

  
  muestra <- D_m[[1]][sample(1:n, minority.less), ]
  rownames(muestra) <- NULL
  
  # Detección de K vecinos
  print("Hallando KNN")
  subset.df = subset(df, select = colnames(df)[colnames(df) != str_weight_name])
  knn = SMOTE.ENC.KNN(subset.df, target, minority.value, vars.numeric, FALSE, k, seed)
  
  # Generación Muestra Sintética
  print("Generando Synthetic")
  to.sint <- subset(df, select = -FACTOR_EXPANSION)
  Synthetic = SyntheticData(df, vars.numeric, target, minority.value, minority.less, muestra, knn, 'FACTOR_EXPANSION', seed, cl)
  
  # Se detiene el trabajo en paralelo
  stopCluster(cl)
  
  # Concatenación de Resultados
  resampled_data = rbind(df, Synthetic)
  
  return(resampled_data)
}