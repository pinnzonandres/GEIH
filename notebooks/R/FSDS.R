# Notebook con la función para generar la muestra junto a los pesos
## Carga de Librerias
library(tidyverse)
library(parallel)
## Función de cálculo
fsds_sample <- function(indice, factor_expansion, N, n, seed, cl){
  restante = N-n
  limite = ifelse(restante>50*n, 50*n, restante)
  set.seed(seed)
  
  freq <- table(indice)
  sumatoria <- sum(freq[as.character(indice)] * factor_expansion[indice])
  pesos_ajustados <- (N* factor_expansion[indice]*freq[as.character(indice)])/sumatoria
  
  draw <- sample(indice, size = limite, replace = TRUE, prob = pesos_ajustados)
  new_indice = c(indice, draw)
  print("Pseudo Poblation")

  calcular_pi <- function(i, new_indice, pesos_ajustados, n, restante) {
    new_freq <- table(new_indice[1:i])
    numerador <- pesos_ajustados[as.character(new_indice[i])] - 1 + (new_freq[as.character(new_indice[i])]) * (restante / n)
    denominador <- restante + ((i - n) - 1) * restante / n
    p_i <- denominador / numerador
    return(p_i)
  }

  p_i = unlist(parLapply(cl, (n + 1):(limite + n), calcular_pi, new_indice, pesos_ajustados,n, restante))
  #p_i = unlist(lapply((n+1):(limite+n), function(i){
  #  new_freq = table(new_indice[1:i])
  #  numerador = pesos_ajustados[as.character(new_indice[i])]-1 +
  #   (new_freq[as.character(new_indice[i])])*(restante/(n))
  #  denominador = restante + ((i-n)-1)*restante/n
  #  p_i = (denominador/numerador)
  #  return(p_i)
  #}))
  
  new_weight = c(pesos_ajustados, p_i)
  
  pseudo = data.frame(
    indice = new_indice,
    weight = new_weight
  )
  
  D_m <- pseudo[sample(1:nrow(pseudo),n),]

  return(D_m)
}

FSDS <- function(df, factor_expansion, N, m, n, seed, cl){
  set.seed(seed)
  indices <- lapply(1:m, function(i) {
  sample(as.numeric(rownames(df)), size = n, replace = TRUE)
})

  D_m <- lapply(1:m, function(i) {
  fsds_sample(indices[[i]], factor_expansion, N, n, seed, cl)
})
  
  return(D_m)
}

