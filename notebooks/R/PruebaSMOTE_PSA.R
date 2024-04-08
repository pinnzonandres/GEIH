# Script que contiene los resultados de los métodos propuestos

# Carga de librerias
library(survey)
library(dplyr)
library(MatchIt)
library(caret)

# Carga del Script con las funciones de balanceo
source("C:\\Users\\andre\\OneDrive\\Escritorio\\Proyecto de Grado\\notebooks\\R\\Balanceo.R")

data <- read.csv(
  "C:/Users/andre/OneDrive/Escritorio/Proyecto de Grado/result/db_feature_selection/FS_Enero.csv"
)

# Definición de valores base
seed = 3
set.seed(seed)
size = 0.8
target = "ACTIVIDAD_OCUPADA_ULTIMA_SEMANA"
minority.value = 0
k = 5

# Variables numéricas
vars.numeric <- c("EDAD")
data[vars.numeric] <- scale(data[vars.numeric])

## Train Test split
train_index <- sample(
  nrow(data),
  size = size * nrow(data),
  replace = FALSE
  )

train <- data[train_index, ]
test <- data[-train_index, ]

rownames(train) <- NULL
rownames(test) <- NULL

## Ajuste del conjunto de entrenamiento para la generación de las muestras sintéticas
train.nowt <- subset(train, select = -FACTOR_EXPANSION)

train.nowt$ACTIVIDAD_OCUPADA_ULTIMA_SEMANA = as.factor(train.nowt$ACTIVIDAD_OCUPADA_ULTIMA_SEMANA)

# Generación de muestras sintéticas
# SMOTE.ENC
synt.smote = SMOTE_ENC(
  df = train.nowt,
  target = target,
  minority.value = minority.value,
  vars.numeric = vars.numeric,
  k = k,
  parallel = TRUE,
  seed = seed
)

# ADASYN.ENC
synt.adasyn = ADASYN.ENC(
  df = train.nowt,
  target = target,
  minority.value = minority.value,
  vars.numeric = vars.numeric,
  k = k,
  parallel = TRUE,
  seed = seed
  )

# WSMOTE.ENC
factor.expansion = train$FACTOR_EXPANSION
synt.wsmote = WSMOTE.ENC(
  df = train.nowt,
  factor_expansion = factor.expansion,
  target = target,
  minority.value = minority.value,
  vars.numeric = vars.numeric,
  k = k,
  parallel = TRUE,
  seed = seed
)

# Ajuste a valores de tipo numérico
synt.smote <- synt.smote %>%
  mutate(across(all_of(vars.numeric), as.numeric))

synt.adasyn <- synt.adasyn %>%
  mutate(across(all_of(vars.numeric), as.numeric))

synt.wsmote <- synt.wsmote %>%
  mutate(across(all_of(vars.numeric), as.numeric))

match_rows = function(original, sintetico){
  # Ajuste de Pesos para los datos sintéticos
  sintetico$FACTOR_EXPANSION <- 1
  
  # Definición de Indicador de pertenecer a la muestra no probabilística
  sintetico$Pertenece <- 1
  original$Pertenece <- 0
  
  resample <- rbind(original, sintetico)
  
  # Algoritmo Match
  match.it <- matchit(
    Pertenece~.-FACTOR_EXPANSION,
    data = resample,
    method = "nearest",
    ratio = 1
    )
  
  df.match <- match.data(match.it)[1:ncol(resample)]
  
  return(df.match)
}

## Conjuntos de datos match

match.smote <- match_rows(
  original = train,
  sintetico = synt.smote
)
match.smote$EDAD <- as.numeric(match.smote$EDAD)


match.adasyn <- match_rows(
  original = train,
  sintetico = synt.adasyn
)
match.adasyn$EDAD <- as.numeric(match.adasyn$EDAD)

match.wsmote <- match_rows(
  original = train,
  sintetico = synt.wsmote
)
match.wsmote$EDAD <- as.numeric(match.wsmote$EDAD)

# Ajuste de los diseños que permitiran ajustar los modelos de regresión logística
# SMOTE
design.match.smote <- svydesign(
  ids = ~1,
  data = match.smote,
  weights = ~FACTOR_EXPANSION
  )

nombres_variables <- names(design.match.smote$variables)
variables_a_eliminar <- c('Pertenece', 'FACTOR_EXPANSION')
nombres_variables <- setdiff(nombres_variables, variables_a_eliminar)

formula_auto <- as.formula(paste("Pertenece ~", paste(nombres_variables, collapse = " + ")))

# Ajustar el modelo, excluyendo ACTIVIDAD_OCUPADA_ULTIMA_SEMANA solo de la fórmula
model.smote <- svyglm(formula = formula_auto, 
                design = design.match.smote,
                family = quasibinomial())

# ADASYN
design.match.adasyn <- svydesign(
  ids = ~1,
  data = match.adasyn,
  weights = ~FACTOR_EXPANSION
  )

nombres_variables <- names(design.match.adasyn$variables)
variables_a_eliminar <- c('Pertenece', 'FACTOR_EXPANSION')
nombres_variables <- setdiff(nombres_variables, variables_a_eliminar)

formula_auto <- as.formula(paste("Pertenece ~", paste(nombres_variables, collapse = " + ")))

model.adasyn <- svyglm(formula = formula_auto, 
                design = design.match.adasyn,
                family = quasibinomial())

# WSMOTE
design.match.wsmote <- svydesign(
  ids = ~1,
  data = match.wsmote,
  weights = ~FACTOR_EXPANSION
  )

nombres_variables <- names(design.match.wsmote$variables)
variables_a_eliminar <- c('Pertenece', 'FACTOR_EXPANSION')
nombres_variables <- setdiff(nombres_variables, variables_a_eliminar)

formula_auto <- as.formula(paste("Pertenece ~", paste(nombres_variables, collapse = " + ")))

model.wsmote <- svyglm(formula = formula_auto, 
                       design = design.match.wsmote,
                       family = quasibinomial())

# Calculo de los nuevos pesos
# Smote
p_i.smote <- predict.glm(model.smote, synt.smote, type = "response")
w_i.smote = 1/p_i.smote
synt.smote$FACTOR_EXPANSION <- w_i.smote

# ADASYN
p_i.adasyn <- predict.glm(model.adasyn, synt.adasyn, type = "response")
w_i.adasyn = 1/p_i.adasyn
synt.adasyn$FACTOR_EXPANSION <- w_i.adasyn

# Smote
p_i.wsmote <- predict.glm(model.wsmote, synt.wsmote, type = "response")
w_i.wsmote = 1/p_i.wsmote
synt.wsmote$FACTOR_EXPANSION <- w_i.wsmote

# Ajuste de los pesos de muestreo
ajuste.pesos <- function(
    original,
    sintetico){
  nw_sintetico = nrow(sintetico)
  nw_original = nrow(original)
  
  sum_wt_sintetico = sum(sintetico$FACTOR_EXPANSION)
  sum_wt_original = sum(original$FACTOR_EXPANSION)
  
  w_s_sintetico = sintetico$FACTOR_EXPANSION * ((nw_sintetico)/(nw_sintetico + nw_original)) *(sum_wt_original/sum_wt_sintetico) 
  w_s_original = original$FACTOR_EXPANSION * (nw_original/(nw_original+nw_sintetico))
  
  original$FACTOR_EXPANSION = w_s_original
  sintetico$FACTOR_EXPANSION = w_s_sintetico
  
  result = rbind(original, sintetico)
  
  return(result)
}

#SMOTE
smote.psa <- ajuste.pesos(
  original = train,
  sintetico = synt.smote
)

#Adasyn
adasyn.psa <- ajuste.pesos(
  original = train,
  sintetico = synt.adasyn
)

#WSMOTE
wsmote.psa <- ajuste.pesos(
  original = train,
  sintetico = synt.wsmote
)

# Ajuste de conjuntos de datos balanceados sin ajuste de pesos por PSA
# SMOTE
smote.nopsa <- synt.smote
smote.nopsa$FACTOR_EXPANSION <- 1
smote.nopsa <- rbind(train, smote.nopsa)

#ADASYN
adasyn.nopsa <- synt.adasyn
adasyn.nopsa$FACTOR_EXPANSION <- 1
adasyn.nopsa <- rbind(train, adasyn.nopsa)

#WSMOTE
wsmote.nopsa <- synt.wsmote
wsmote.nopsa$FACTOR_EXPANSION <- 1
wsmote.nopsa <- rbind(train, wsmote.nopsa)


# Variables para el modelo de regresión
nombres_variables <- names(train)
variables_a_eliminar <- c('ACTIVIDAD_OCUPADA_ULTIMA_SEMANA', 'FACTOR_EXPANSION','Pertenece')
nombres_variables <- setdiff(nombres_variables, variables_a_eliminar)
formula_auto <- as.formula(paste("ACTIVIDAD_OCUPADA_ULTIMA_SEMANA ~", paste(nombres_variables, collapse = " + ")))


## Generación de Modelos de Regresión Logística
## Conjunto Original
original.design <- svydesign(ids = ~1, data = train, weights = ~FACTOR_EXPANSION)
original.model <- svyglm(formula = formula_auto, 
                          design = original.design,
                          family = quasibinomial())

# SMOTE
# Con PSA
smote.design.psa <- svydesign(ids = ~1, data = smote.psa, weights = ~FACTOR_EXPANSION)
smote.model.psa <- svyglm(formula = formula_auto, 
                       design = smote.design.psa,
                       family = quasibinomial())

# Sin PSA
smote.design.nopsa <- svydesign(ids = ~1, data = smote.nopsa, weights = ~FACTOR_EXPANSION)
smote.model.nopsa <- svyglm(formula = formula_auto, 
                          design = smote.design.nopsa,
                          family = quasibinomial())


# ADASYN
# Con PSA
adasyn.design.psa <- svydesign(ids = ~1, data = adasyn.psa, weights = ~FACTOR_EXPANSION)
adasyn.model.psa <- svyglm(formula = formula_auto, 
                          design = adasyn.design.psa,
                          family = quasibinomial())

# Sin PSA
adasyn.design.nopsa <- svydesign(ids = ~1, data = adasyn.nopsa, weights = ~FACTOR_EXPANSION)
adasyn.model.nopsa <- svyglm(formula = formula_auto, 
                            design = adasyn.design.nopsa,
                            family = quasibinomial())
# WSMOTE
# Con PSA
wsmote.design.psa <- svydesign(ids = ~1, data = wsmote.psa, weights = ~FACTOR_EXPANSION)
wsmote.model.psa <- svyglm(formula = formula_auto, 
                          design = wsmote.design.psa,
                          family = quasibinomial())

# Sin PSA
wsmote.design.nopsa <- svydesign(ids = ~1, data = wsmote.nopsa, weights = ~FACTOR_EXPANSION)
wsmote.model.nopsa <- svyglm(formula = formula_auto, 
                            design = wsmote.design.nopsa,
                            family = quasibinomial())

# Evaluación de los modelos
original.y_pred <- ifelse(predict.glm(original.model, newdata = test, type = "response") > 0.5, 1, 0)
smote.psa.y_pred <- ifelse(predict.glm(smote.model.psa, newdata = test, type = "response") > 0.5, 1, 0)
smote.nopsa.y_pred <- ifelse(predict.glm(smote.model.nopsa, newdata = test, type = "response") > 0.5, 1, 0)
adasyn.psa.y_pred <- ifelse(predict.glm(adasyn.model.psa, newdata = test, type = "response") > 0.5, 1, 0)
adasyn.nopsa.y_pred <- ifelse(predict.glm(adasyn.model.nopsa, newdata = test, type = "response") > 0.5, 1, 0)
wsmote.psa.y_pred <- ifelse(predict.glm(wsmote.model.psa, newdata = test, type = "response") > 0.5, 1, 0)
wsmote.nopsa.y_pred <- ifelse(predict.glm(wsmote.model.nopsa, newdata = test, type = "response") > 0.5, 1, 0)




metrics_list <- lapply(
  list(
    original.y_pred,
    smote.nopsa.y_pred,
    smote.psa.y_pred,
    adasyn.nopsa.y_pred,
    adasyn.psa.y_pred,
    wsmote.nopsa.y_pred,
    wsmote.psa.y_pred
    ), function(predictions) {
      cm <- confusionMatrix(table(predictions, test$ACTIVIDAD_OCUPADA_ULTIMA_SEMANA), positive = "1")
      acc <- cm$overall["Accuracy"]
      metricas <- cm$byClass
  data.frame(
    Accuracy = round(acc,3),
    Precision = round(metricas["Precision"],3),
    Recall = round(metricas["Recall"],3),
    Specificity = round(metricas["Specificity"],3),
    Balanced_Accuracy = round(metricas["Balanced Accuracy"],3),
    F1_Score = round(metricas["F1"],3)
  )
})

metrics_df <- do.call(rbind, metrics_list)
rownames(metrics_df) <- c("Original", "SMOTE-ENC no PSA", "SMOTE-ENC & PSA",
                          "ADASYN no PSA", "ADASYN & PSA", "WSMOTE nO PSA",
                          "WSMOTE & PSA")
print(metrics_df)

confusionMatrix(table(wsmote.nopsa.y_pred, test$ACTIVIDAD_OCUPADA_ULTIMA_SEMANA), positive = "")
