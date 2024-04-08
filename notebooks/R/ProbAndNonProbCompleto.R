# Script que contiene los resultados de los métodos propuestos

# Carga de librerias
library(survey)
library(dplyr)
library(MatchIt)
library(pROC)
library(cvms)
library(tibble)
library(gridExtra)

# Carga del Script con las funciones de balanceo
source("C:\\Users\\andre\\OneDrive\\Escritorio\\Proyecto de Grado\\notebooks\\R\\Balanceo.R")

data <- read.csv(
  "C:/Users/andre/OneDrive/Escritorio/Proyecto de Grado/result/db_feature_selection/FS_Enero_Completo.csv"
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
full.smote <- ajuste.pesos(
  original = train,
  sintetico = synt.smote
)

#Adasyn
full.adasyn <- ajuste.pesos(
  original = train,
  sintetico = synt.adasyn
)

#WSMOTE
full.wsmote <- ajuste.pesos(
  original = train,
  sintetico = synt.wsmote
)

# Variables para el modelo de regresión
nombres_variables <- names(train)
variables_a_eliminar <- c('ACTIVIDAD_OCUPADA_ULTIMA_SEMANA', 'FACTOR_EXPANSION','Pertenece')
nombres_variables <- setdiff(nombres_variables, variables_a_eliminar)
formula_auto <- as.formula(paste("ACTIVIDAD_OCUPADA_ULTIMA_SEMANA ~", paste(nombres_variables, collapse = " + ")))


## Generación de Modelos de Regresión Logística
# SMOTE
full.design.smote <- svydesign(ids = ~1, data = full.smote, weights = ~FACTOR_EXPANSION)

full.model.smote <- svyglm(formula = formula_auto, 
                           design = full.design.smote,
                           family = quasibinomial())

# ADASYN
full.design.adasyn <- svydesign(ids = ~1, data = full.adasyn, weights = ~FACTOR_EXPANSION)

full.model.adasyn <- svyglm(formula = formula_auto, 
                            design = full.design.adasyn,
                            family = quasibinomial())

# WSMOTE
full.design.wsmote <- svydesign(ids = ~1, data = full.wsmote, weights = ~FACTOR_EXPANSION)

full.model.wsmote <- svyglm(formula = formula_auto, 
                            design = full.design.wsmote,
                            family = quasibinomial())

# Evaluación de los modelos
y_pred.smote <- ifelse(predict.glm(full.model.smote, newdata = test, type = "response") > 0.5, 1, 0)
y_pred.adasyn <- ifelse(predict.glm(full.model.adasyn, newdata = test, type = "response") > 0.5, 1, 0)
y_pred.wsmote <- ifelse(predict.glm(full.model.wsmote, newdata = test, type = "response") > 0.5, 1, 0)


metrics_list <- lapply(list(y_pred.smote, y_pred.adasyn, y_pred.wsmote), function(predictions) {
  cm <- table(predictions, test$ACTIVIDAD_OCUPADA_ULTIMA_SEMANA)
  
  accuracy <- (cm[4] + cm[1])/sum(cm); accuracy
  precision <- cm[4]/(cm[4] + cm[3]); precision
  recall <- cm[4]/(cm[4] + cm[2]); recall
  f1_score <- 2*(precision*recall)/(precision+recall); f1_score
  data.frame(
    Accuracy = accuracy,
    Precision = precision,
    Recall = recall,
    F1_Score = f1_score
  )
})

metrics_df <- do.call(rbind, metrics_list)
rownames(metrics_df) <- c("SMOTE", "ADASYN", "WSMOTE")
print(metrics_df)

cm <- function(predicion, y_test){
  result <- tibble("Prediction" = predicion,
                   "Target" = y_test)
  ploted = plot_confusion_matrix(as_tibble(table(result)),
                                 target_col = "Target",
                                 prediction_col = "Prediction",
                                 counts_col = "n")
  return(ploted)
}


plot.smote <- cm(y_pred.smote, test$ACTIVIDAD_OCUPADA_ULTIMA_SEMANA)
plot.adasyn <- cm(y_pred.adasyn, test$ACTIVIDAD_OCUPADA_ULTIMA_SEMANA)
plot.wsmote <- cm(y_pred.wsmote, test$ACTIVIDAD_OCUPADA_ULTIMA_SEMANA)

grid.arrange(plot.smote, plot.adasyn, plot.wsmote, ncol = 3)

roc1 <- roc(y_pred.smote, test$ACTIVIDAD_OCUPADA_ULTIMA_SEMANA)
roc2 <- roc(y_pred.adasyn, test$ACTIVIDAD_OCUPADA_ULTIMA_SEMANA)
roc3 <- roc(y_pred.wsmote, test$ACTIVIDAD_OCUPADA_ULTIMA_SEMANA)

auc_result <- data.frame(Modelo = c("SMOTE", "ADASYN", "WSMOTE"), auc = c(roc1$auc, roc2$auc, roc3$auc))
print(auc_result)



## Natural WSMOTE
natural.smote <- synt.wsmote
natural.smote$FACTOR_EXPANSION <- 1

full.natural.smote <- ajuste.pesos(
  original = train,
  sintetico = natural.smote
)

full.design.natural <- svydesign(ids = ~1, data = full.natural.smote, weights = ~FACTOR_EXPANSION)

full.model.natural <- svyglm(formula = formula_auto, 
                             design = full.design.natural,
                             family = quasibinomial())

# Evaluación de los modelos
y_pred.natural <- ifelse(predict.glm(full.model.natural, newdata = test, type = "response") > 0.5, 1, 0)

table(y_pred.natural, test$ACTIVIDAD_OCUPADA_ULTIMA_SEMANA)

roc(y_pred.natural, test$ACTIVIDAD_OCUPADA_ULTIMA_SEMANA)


# Modelo sin Balanceo
full.design.natural <- svydesign(ids = ~1, data = train, weights = ~FACTOR_EXPANSION)

full.model.natural <- svyglm(formula = formula_auto, 
                             design = full.design.natural,
                             family = quasibinomial())

# Evaluación de los modelos
y_pred.natural <- ifelse(predict.glm(full.model.natural, newdata = test, type = "response") > 0.5, 1, 0)

table(y_pred.natural, test$ACTIVIDAD_OCUPADA_ULTIMA_SEMANA)

roc(y_pred.natural, test$ACTIVIDAD_OCUPADA_ULTIMA_SEMANA)
