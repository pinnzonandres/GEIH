# Notebook con la función para generar la muestra junto a los pesos
## Carga de Librerias
library(tidyverse)

## Función de cálculo
fsds_DM <- function(indice, factor_expansion, N, n, restante, seed) {
  set.seed(seed)
  # Paso 1: Extracción de muestras y cálculo del nuevo peso
  freq <- table(indice)
  sumatoria <- sum(freq[as.character(indice)] * factor_expansion[indice])
  pesos_ajustados <- (N* factor_expansion[indice]*freq[as.character(indice)])/sumatoria
  
  # Paso 2: Selección de datos con probabilidad ajustada
  new_indice <- indice
  new_weight = pesos_ajustados
  for (i in 1:restante){
    draw = sample(indice, size = 1, replace = TRUE, prob = pesos_ajustados)
    p_i = (pesos_ajustados[as.character(draw)] - 1 + 
             (table(new_indice)[as.character(draw)]*(restante)/n))/(N-n + ((i -1)*(restante)/n))
    new_weight = c(new_weight, p_i)
    new_indice = c(new_indice, draw)
  }
  
  # Paso 3: Creación de la pseudo-población y extración de la muestra final
  pseudo_m <- data.frame(cbind(new_indice, new_weight))
  rownames(pseudo_m) <- NULL
  D_m <- pseudo_m[sample(1:n),]
  
  # Regresar la muestra final
  return(D_m)
}

FSDS <- function(df, factor_expansion, N, m, n, seed){
  set.seed(seed)
  indices <- lapply(1:m, function(i) {
    sample(1:nrow(df), size = n, replace = TRUE)
  })
  restante = N - m
  
  D_m <- lapply(1:m, function(i) {
    fsds_DM(indices[[i]], factor_expansion, N, n, restante, seed)
  })
  
  return(D_m)
}

