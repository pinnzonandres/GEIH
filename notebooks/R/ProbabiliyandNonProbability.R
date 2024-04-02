## Prueba Tipos de SMOTE con ajuste de pesos por regresión logística
# 1. Versión - SMOTE.ENC Clásico
# 2. Versión - Uso sintético de SMOTE.ENC con ADASYN
# 3. Versión - SWSMOTEENC

library(survey)
library(dplyr)

source("C:\\Users\\andre\\OneDrive\\Escritorio\\Proyecto de Grado\\notebooks\\R\\oversampling.R")
source("C:\\Users\\andre\\OneDrive\\Escritorio\\Proyecto de Grado\\notebooks\\R\\Extras.R")


## Dataframe Base
data <- read.csv("C:/Users/andre/OneDrive/Escritorio/Proyecto de Grado/result/db_agrupado/Enero_Agrupado.csv", sep=";")
data <- subset(data, select = -DIRECTORIO)

# Plantación de Semilla y Valores Base
seed = 3
set.seed(seed)
size = 0.7
target = "ACTIVIDAD_OCUPADA_ULTIMA_SEMANA"
minority.value = 0
k = 5

## Train Test split
train_index <- sample(nrow(data), size = size * nrow(data), replace = FALSE)

train <- data[train_index, ]
test <- data[-train_index, ]

rownames(train) <- NULL
rownames(test) <- NULL


## Ajuste del conjunto de entrenamiento para la generación de las muestras sintéticas
train.nowt <- subset(train, select = -FACTOR_EXPANSION)

train.nowt$ACTIVIDAD_OCUPADA_ULTIMA_SEMANA = as.factor(train.nowt$ACTIVIDAD_OCUPADA_ULTIMA_SEMANA)
vars.numeric <- names(train.nowt)[sapply(train.nowt, is.numeric)]


#### 1. Versión
### Método SMOTE ENC
synt.smote = SMOTE_ENC(train.nowt, target, minority.value, vars.numeric, k , seed)
synt.smote <- synt.smote %>% mutate(across(all_of(vars.numeric), as.numeric))


# Ajuste de Pesos para los datos sintéticos
synt.smote$FACTOR_EXPANSION = 1

train.smote <- train

train.smote$Pertenece = 0
synt.smote$Pertenece = 1

resample.smote = rbind(train.smote, synt.smote)

ids = seq(1, nrow(resample.smote))

design.smote <- svydesign(ids = ids, data = resample.smote, weights = resample.smote$FACTOR_EXPANSION)

model.smote = svyglm(formula = Pertenece ~ ZONA_TERRITORIAL + SEXO + EDAD + 
                 ETNIA + ESTADO_CIVIL + LEER_ESCRIBIR + ACTUALMENTE_ESTUDIA + 
                 MAYOR_NIVEL_EDUCATIVO + ORIENTACION_SEXUAL + GENERO + TIPO_VIVIENDA + 
                 ESTRATO_ENERGIA_ELECTRICA + NUMERO_HOGARES_VIVIENDA + NUMERO_CUARTOS_VIVIENDA + 
                 NUMERO_CUARTOS_DORMIR + DONDE_OBTIENE_AGUA + DONDE_PREPARA_ALIMENTOS + 
                 TIPO_OCUPACION_VIVIENDA + NUMERO_PERSONAS_HOGAR + DISCAPACIDAD, 
               design = design.smote,
               family = quasibinomial())

p_i.smote <- predict(model.smote, synt.smote, type = "response")
w_i.smote = 1/p_i.smote

synt.smote$FACTOR_EXPANSION <- w_i.smote

## Ajuste de los pesos
n_s_x = nrow(synt.smote)
n_s = nrow(train.smote)

w_i_x <- synt.smote$FACTOR_EXPANSION * ((n_s_x)/(n_s_x + n_s)) *((sum(train.smote$FACTOR_EXPANSION))/(sum(synt.smote$FACTOR_EXPANSION)))
w_i <- train.smote$FACTOR_EXPANSION * ((n_s)/(n_s + n_s_x))

synt.smote$FACTOR_EXPANSION <- w_i_x
train.smote$FACTOR_EXPANSION <- w_i

full.smote <- rbind(train.smote, synt.smote)

# Evaluación del Modelo
smote.glm <- survey_model(full.smote, test, "FACTOR_EXPANSION")



#### 2. Versión
### Método ADASYN
synt.adasyn = ADASYN(train.nowt, target, minority.value, vars.numeric, k , seed)
synt.adasyn <- synt.adasyn %>% mutate(across(all_of(vars.numeric), as.numeric))


# Ajuste de Pesos para los datos sintéticos
synt.adasyn$FACTOR_EXPANSION = 1

train.adasyn <- train

train.adasyn$Pertenece = 0
synt.adasyn$Pertenece = 1

resample.adasyn = rbind(train.adasyn, synt.adasyn)

ids = seq(1, nrow(resample.adasyn))

design.adasyn <- svydesign(ids = ids, data = resample.adasyn, weights = resample.adasyn$FACTOR_EXPANSION)

model.adasyn = svyglm(formula = Pertenece ~ ZONA_TERRITORIAL + SEXO + EDAD + 
                       ETNIA + ESTADO_CIVIL + LEER_ESCRIBIR + ACTUALMENTE_ESTUDIA + 
                       MAYOR_NIVEL_EDUCATIVO + ORIENTACION_SEXUAL + GENERO + TIPO_VIVIENDA + 
                       ESTRATO_ENERGIA_ELECTRICA + NUMERO_HOGARES_VIVIENDA + NUMERO_CUARTOS_VIVIENDA + 
                       NUMERO_CUARTOS_DORMIR + DONDE_OBTIENE_AGUA + DONDE_PREPARA_ALIMENTOS + 
                       TIPO_OCUPACION_VIVIENDA + NUMERO_PERSONAS_HOGAR + DISCAPACIDAD, 
                     design = design.adasyn,
                     family = quasibinomial())

p_i.adasyn <- predict(model.adasyn, synt.adasyn, type = "response")
w_i.adasyn = 1/p_i.adasyn

synt.adasyn$FACTOR_EXPANSION <- w_i.adasyn

## Ajuste de los pesos
n_s_x = nrow(synt.adasyn)
n_s = nrow(train.adasyn)

w_i_x <- synt.adasyn$FACTOR_EXPANSION * ((n_s_x)/(n_s_x + n_s)) *((sum(train.adasyn$FACTOR_EXPANSION))/(sum(synt.adasyn$FACTOR_EXPANSION)))
w_i <- train.adasyn$FACTOR_EXPANSION * ((n_s)/(n_s + n_s_x))

synt.adasyn$FACTOR_EXPANSION <- w_i_x
train.adasyn$FACTOR_EXPANSION <- w_i

full.adasyn <- rbind(train.adasyn, synt.adasyn)

# Evaluación del Modelo
adasyn.glm <- survey_model(full.adasyn, test, "FACTOR_EXPANSION")


#### 3. Versión
### Método SWSMOTEENC
factor.expansion <- train$FACTOR_EXPANSION
synt.swsmoteenc = SWSMOTEENC(train.nowt, factor.expansion, target, minority.value, vars.numeric, k , seed)
synt.swsmoteenc <- synt.swsmoteenc %>% mutate(across(all_of(vars.numeric), as.numeric))


# Ajuste de Pesos para los datos sintéticos
synt.swsmoteenc$FACTOR_EXPANSION = 1

train.swsmoteenc <- train

train.swsmoteenc$Pertenece = 0
synt.swsmoteenc$Pertenece = 1

resample.swsmoteenc = rbind(train.swsmoteenc, synt.swsmoteenc)

ids = seq(1, nrow(resample.swsmoteenc))

design.swsmoteenc <- svydesign(ids = ids, data = resample.swsmoteenc, weights = resample.swsmoteenc$FACTOR_EXPANSION)

model.swsmoteenc = svyglm(formula = Pertenece ~ ZONA_TERRITORIAL + SEXO + EDAD + 
                        ETNIA + ESTADO_CIVIL + LEER_ESCRIBIR + ACTUALMENTE_ESTUDIA + 
                        MAYOR_NIVEL_EDUCATIVO + ORIENTACION_SEXUAL + GENERO + TIPO_VIVIENDA + 
                        ESTRATO_ENERGIA_ELECTRICA + NUMERO_HOGARES_VIVIENDA + NUMERO_CUARTOS_VIVIENDA + 
                        NUMERO_CUARTOS_DORMIR + DONDE_OBTIENE_AGUA + DONDE_PREPARA_ALIMENTOS + 
                        TIPO_OCUPACION_VIVIENDA + NUMERO_PERSONAS_HOGAR + DISCAPACIDAD, 
                      design = design.swsmoteenc,
                      family = quasibinomial())

p_i.swsmoteenc <- predict(model.swsmoteenc, synt.swsmoteenc, type = "response")
w_i.swsmoteenc = 1/p_i.swsmoteenc

synt.swsmoteenc$FACTOR_EXPANSION <- w_i.swsmoteenc

## Ajuste de los pesos
n_s_x = nrow(synt.swsmoteenc)
n_s = nrow(train.swsmoteenc)

w_i_x <- synt.swsmoteenc$FACTOR_EXPANSION * ((n_s_x)/(n_s_x + n_s)) *((sum(train.swsmoteenc$FACTOR_EXPANSION))/(sum(synt.swsmoteenc$FACTOR_EXPANSION)))
w_i <- train.swsmoteenc$FACTOR_EXPANSION * ((n_s)/(n_s + n_s_x))

synt.swsmoteenc$FACTOR_EXPANSION <- w_i_x
train.swsmoteenc$FACTOR_EXPANSION <- w_i

full.swsmoteenc <- rbind(train.swsmoteenc, synt.swsmoteenc)

# Evaluación del Modelo
swsmoteenc.glm <- survey_model(full.swsmoteenc, test, "FACTOR_EXPANSION")

