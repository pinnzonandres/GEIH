library(FNN)
library(modeest)
source("C:\\Users\\andre\\OneDrive\\Escritorio\\Proyecto de Grado\\notebooks\\R\\FSDS.R")


# Función para encontrar los k vecinos más cercanos según la métrica dada por SMOTE.ENC
SMOTE.ENC.KNN <- function(df, target, minority_value, k, seed){
  # Plantación de Semilla
  set.seed(seed)
  
  # Detección de Variables Numéricas y Categóricas
  vars.numeric <- names(df)[sapply(df, is.numeric)]
  vars.categoric <- names(df)[sapply(df, is.factor)]
  vars.categoric <- vars.categoric[vars.categoric != target] 
  
  # Escalamiendo de los datos
  #df[vars.numeric] <- lapply(df[vars.numeric], scale)
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
  
  knn = get.knn(df_encoded_numeric, k)$nn.index
  
  return(knn)
}

generacion_muestra <- function(df, target, minority.value, str_weight, k, vars.categoric, vars.numeric, seed){
    # Plantación de Semilla
    set.seed(seed)

    # Detección del conjunto minoritario
    minority.df = df[df[[target]] == minority.value, ]
    
    # Definición de valores necesarios
    S = nrow(df)
    minority.less = S - 2*nrow(minority.df)

    m = 1
    n = S + minority.less
    N = round(sum(df[[str_weight]]),0)

    # Ajuste de Pesos
    ratio.ajuste = S/n
    df[['Adjusted_Weight']] <- df[[str_weight]]*ratio.ajuste

    factor.expansion <- df[['Adjusted_Weight']]

    D_m = FSDS(minority.df, factor.expansion, N, m, n, seed = seed)

    muestra = D_m[[1]][sample(1:n, minority.less),]
    rownames(muestra) <- NULL

    subset.df = subset(df, select = colnames(df)[colnames(df) != str_weight])
    
    # Obtención de los k vecinos más cercanos según la metrica SMOTE ENC
    knn = SMOTE.ENC.KNN(subset.df, target, minority.value ,k, seed)

    # Generación de las muestras sintéticas
    Synthetic <- data.frame(matrix(nrow = minority.less, ncol = ncol(df)))
    colnames(Synthetic) <- colnames(df)
    Synthetic[vars.categoric] = as.factor(Synthetic[vars.categoric])
    
    # Indice que permite recorrer cada registro de la matriz sinteética
    Synthetic.index = 0
    
    for (j in 1:minority.less){
    id_muestra = muestra[j, 'new_indice']
    i = as.integer(rownames(minority.df[id_muestra,]))
        
    k_neighbors = knn[i,]
    
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
    Synthetic[[target]] = minority.value
    Synthetic[['Adjusted_Weight']] = muestra[['new_weight']]
    Synthetic.index = Synthetic.index + 1
}

resample.data = rbind(df, Synthetic)
return(resample.data)
}
