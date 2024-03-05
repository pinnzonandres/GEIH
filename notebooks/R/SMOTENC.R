library(modeest)
library(FNN)

## Definición de la Función SMOTE.ENC
SMOTE.ENC <-function(data, target, minority_value, k, seed){
  # Plantación de Semilla
  set.seed(seed)
  
  # Copia del DataFrame
  df <- data
  # Detección de Variables Numéricas y Categóricas
  vars.numeric <- names(df)[sapply(df, is.numeric)]
  vars.categoric <- names(df)[sapply(df, is.factor)]
  vars.categoric <- vars.categoric[vars.categoric != target] 
  
  # Escalamiendo de los datos
  df[vars.numeric] <- lapply(df[vars.numeric], scale)
  # Simple Encoder para cálculo de distancias
  encoded.categoric = as.data.frame(sapply(df[vars.categoric], as.numeric))
  
  # Detección del Conjunto Minoritario
  minor_df = df[df[[target]] == minority_value, ]
  
  encoded.minority = encoded.categoric[as.integer(rownames(minor_df)),]
  # Variables Necesarias para el cálculo de la distancia para SMOTE ENC
  t = nrow(minor_df)
  s = nrow(df)
  
  ir = t/s
  c = length(vars.numeric)
  m = ifelse(c>0, median(sapply(minor_df[vars.numeric], sd)), 1)
  
  # Función que determina el vector de valores determinado a cada valor de cada variable
  SMOTE.categoric.distance <- function(data, categoric, m){
    
    l_values <- vector(mode = "list", length = length(categoric))
    names(l_values) <- categoric
    
    # Calcular 'l' para cada variable categórica en una sola línea
    for (var in categoric){
      labels <- unique(data[, var])
      e <- sapply(labels, function(l) sum(data[, var] == l))
      e_star <- e * ir
      o <- sapply(labels, function(l) sum(encoded.minority[, var] == l))
      chi <- (o - e_star)/(e_star)
      l <- chi*m
      l[labels]
      l_values[[var]] = l
    }
    
    return(l_values)
  }
  
  # Cálculo de los valores
  values.categoric <- SMOTE.categoric.distance(encoded.categoric, vars.categoric, m)
  
  # Crear una copia de encoded.categoric para mantener el original intacto
  encoded.categoric_updated <- encoded.categoric
  
  # Aplicar la transformación a través de todas las variables categóricas
  encoded.categoric_updated[vars.categoric] <- lapply(names(encoded.categoric[vars.categoric]), function(var) {
    lapply(encoded.categoric[[var]], function(value) {
      values.categoric[[var]][value]
    })
  })
  
  df_encoded <- cbind(df[vars.numeric], encoded.categoric_updated)
  df_encoded = data.frame(df_encoded)
  
  df_encoded_numeric <- apply(df_encoded, 2, as.numeric)
  
  knn = get.knn(df_encoded_numeric, 5)$nn.index
  
  
  ## Cálculo de la cantidad de muestras sinteticas necesarias
  N = s - 2*t

  # Lista de indices de la clase minoritaría sobre la cuál se harán las muestras sintéticas
  index.minority <- as.integer(rownames(minor_df))
  
  if (N <= t){
    new_index = sample(index.minority, N)
  } else {
    repeticiones = floor(N/t)
    new_index = rep(index.minority, repeticiones)
    restantes = N - (repeticiones*t)
    new_index = c(new_index, sample(index.minority, restantes))
  }
  
  # Creación de Matriz donde se almacenará la información
  Synthetic <- data.frame(matrix(nrow = N, ncol = ncol(df)))
  colnames(Synthetic) <- colnames(df)
  Synthetic[vars.categoric] = as.factor(Synthetic[vars.categoric])
  
  # Indice que permite recorrer cada registro de la matriz sinteética
  Synthetic.index = 0
  
  # Bucle donde se creará la muestra sintética
  for (i in new_index){
    # Se cálcula los k vecinos más cercanos para cada registro de la clase minoritaria
    k_neighbors = knn[i,]
    
    # Se toma un valor aleatorio de los vecinos
    nn <- sample(k_neighbors,1)
    
    for (var in vars.numeric){
      diff = df[[var]][nn] - df[[var]][i]
      gap <- runif(1)
      
      Synthetic[[var]][Synthetic.index +1] <- df[[var]][i] + gap*diff
    }
    for (var in vars.categoric){
      value = as.character(mfv(df[[var]][k_neighbors])[1])
      Synthetic[[var]][Synthetic.index+1] = value
    }
    Synthetic[[target]] = minority_value
    Synthetic.index = Synthetic.index + 1
  }
  
  resample.data = rbind(df, Synthetic)
  
  return(resample.data)
  
}

