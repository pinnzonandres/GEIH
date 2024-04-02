library(sampling)
library(pps)
library(polyapost)
library(survey)
library(parallel)

df <- read.csv("C:\\Users\\andre\\OneDrive\\Escritorio\\Proyecto de Grado\\result\\db_agrupado\\Enero_Agrupado.csv")

fsds.replicates <- function(df, R, seed){
  # Plantación de Semilla
  set.seed(seed)
  
  # Ajuste Poblacional
  N <- round(sum(df$FACTOR_EXPANSION),0)
  
  ## Modelo dentro de la df
  ids.df <- seq(1, nrow(df))
  
  # Diseño de Muestro
  design.df <- svydesign(ids = ids.df, data = df, weights = df$FACTOR_EXPANSION)
  
  # Creación de dfs a través del método Boostrap
  dsgn.rw <- as.svrepdesign(design = design.df, type = "subbootstrap", replicates = R)
  
  # Extración de los pesos
  repwt <- as.matrix(dsgn.rw$repweights)
  
  ## Cálculo de las pseudopoblaciones y dfs con respectivos pesos
  # Cálculo de los pesos para la primera df bootstrap
  repwt <- as.matrix(dsgn.rw$repweights); repwt
  
  n_cores <- detectCores()
  cl <- makeCluster(n_cores)
  
  D_m <-parLapply(cl,1:R, function(i){
    wts = repwt[,i]*df$FACTOR_EXPANSION
    wtind <- wts!=0
    wts <- wts[wtind]
    wts <- (N)*(wts/sum(wts))
    
    # Selección de los indices según la df bootstrap
    polyaY2 <- as.numeric(rownames(df[wtind, ]))
    
    # Establezco el tamaño resultante de mi muestreo bootstrap
    bootsamp_size <- length(wts)
    
    # Hago la selección de los N-n draws ajustado a los pesos calculados
    pop_synth <- c(polyapost::wtpolyap(polyaY2, wts, N - bootsamp_size))
    
    
    ## Se calcula la probabilidad de Selección
    tab = tabulate(pop_synth)
    proba = tab/N
    
    
    sample_synth <-sample(pop_synth, nrow(df), replace = FALSE)
    
    pi = proba[sample_synth]
    wt = 1/pi
    
    adjustment = N/sum(wt)
    
    wtad <- wt*adjustment
    
    result <- data.frame(value = sample_synth, wt =  wtad)
    return(result)
  })
  
  stopCluster(cl)
  
  return(D_m)
}

D_m <- fsds.replicates(df, 100, 123)


index <- matrix(data = NA, nrow = nrow(df), ncol = 100)
wts <- matrix(data = NA, nrow = nrow(df), ncol = 100)

for (i in 1:100){
  index[,i] = D_m[[i]]$value
  wts[,i] = D_m[[i]]$wt
}

df[index[1,],'EDAD']

wts[1,]


median(df[index[1,],'EDAD'])

df[index[1,], 'EDAD']


