# Idea de Proyecto Tipo Regresión
## En este notebook se va a realizar un experimento, donde se va a realizar lo siguiente:
### 1. Generar las S resamples sintéticas a través del método base SMOTE.ENC
### 2. Cambiar el valor de Factor de Expansión como el inverso multiplicativo
### 3. Ajustar un modelo de regresión Beta para estimar el 1/factor de expansión según las variables del dataset
### 4. Estimar 1/Factor de Expansión para la resample sintética
### 5. Ajustar el factor de expansión a la proporción de la estimación de la población

#Paquetes necesarios
library(survey)
library(mlbench)
library(caret)
library(pROC)
library(Metrics)
library(mice)

#Función Fuente
source("C:\\Users\\andre\\OneDrive\\Escritorio\\Proyecto de Grado\\notebooks\\R\\SMOTE_ENC.R")
source("C:\\Users\\andre\\OneDrive\\Escritorio\\Proyecto de Grado\\notebooks\\R\\Extras.R")
source("C:\\Users\\andre\\OneDrive\\Escritorio\\Proyecto de Grado\\notebooks\\R\\Balanceo.R")


## Carga de Datos
df <- read.csv("C:\\Users\\andre\\OneDrive\\Escritorio\\Proyecto de Grado\\result\\db_agrupado\\Enero_Agrupado.csv", encoding="LATIN1", sep=";")
df <- subset(df, select = -DIRECTORIO)

## Modelos de Regresión para estimar el factor de expansión
## Regresión Gamma
df.lm <- subset(df, select = -ACTIVIDAD_OCUPADA_ULTIMA_SEMANA)
mod.gamma <-glm(FACTOR_EXPANSION~., data= df.lm, family = Gamma(link = "log"))

## Regresión Multilineal
## Regresión 
df.lm <- subset(df, select = -ACTIVIDAD_OCUPADA_ULTIMA_SEMANA)
mod.lm <-lm(FACTOR_EXPANSION~., data= df.lm)

###
## Selección de la muestra de entrenamiento y prueba
## Muestreo de Diseño para estimación de la regresión logística 
seed = 3
set.seed(seed)
size = 0.75
train_index <- sample(nrow(df), size = size * nrow(df), replace = FALSE)
train.df <- df[train_index, ]
test.df <- df[-train_index, ]

rownames(train.df) <- NULL
rownames(test.df) <- NULL

##
### Ajuste del conjunto de entrenamiento para la generación de las muestras sintéticas
train.df.sint <- subset(train.df, select = -FACTOR_EXPANSION)

train.df.sint$ACTIVIDAD_OCUPADA_ULTIMA_SEMANA = as.factor(train.df.sint$ACTIVIDAD_OCUPADA_ULTIMA_SEMANA)
vars.numeric <- names(train.df.sint)[sapply(train.df.sint, is.numeric)]

# Valores Base para el trabajo de Sobremuestreo
target = "ACTIVIDAD_OCUPADA_ULTIMA_SEMANA"
minority.value = 0
k = 5 

## Version 1 (SMOTE.ENC)
Synthetic = SMOTE_ENC(train.df.sint, target, minority.value, vars.numeric, k , seed)
Synthetic <- Synthetic %>% mutate(across(all_of(vars.numeric), as.numeric))


# Adición del Factor de Expansión 
## Modelo Gamma
Synt.Wt.gamma = predict(mod.gamma, Synthetic)
Synthetic.gamma = Synthetic
###
Synthetic.gamma$FACTOR_EXPANSION = 1

train.df$Pertenece = 1
Synthetic.gamma$Pertenece = 0
resample.gamma = rbind(train.df, Synthetic.gamma)

# Modelo de regresión binaria
modelo <- glm(Pertenece ~ ., data = resample.gamma, family = "binomial")

summary(modelo)
predict(modelo, type="response")
step(modelo)


# Ajuste Proporción Factor de Expansión
ratio = nrow(train.df)/(nrow(resample.gamma))

resample.gamma$FACTOR_EXPANSION = resample.gamma$FACTOR_EXPANSION*ratio


modelo.gamma <- survey_model(resample.gamma, test.df, "FACTOR_EXPANSION")


## Segundo Caso, utilización del método de imputación MICE para estimar los pesos de muestreo

Synthetic$FACTOR_EXPANSION <- NA
resample <- rbind(train.df, Synthetic)


imp = resample %>%
  mice(m = 5, maxit = 30) %>%
  complete(action = 1, seed = seed)

imp$FACTOR_EXPANSION <- imp$FACTOR_EXPANSION*ratio

modelo.mice <- survey_model(imp, test.df, "FACTOR_EXPANSION")


## Tercer Caso, método_incompleto SMOTE.FSDS
sintetic.fsds <- SMOTE.ENC.FSDS(train.df, target, minority.value, "FACTOR_EXPANSION", 5, vars.numeric, seed)
sintetic.fsds <- sintetic.fsds %>% mutate(across(all_of(vars.numeric), as.numeric))

sintetic.fsds$FACTOR_EXPANSION = sintetic.fsds$FACTOR_EXPANSION*ratio

modelo.fsds <- survey_model(sintetic.fsds, test.df, "FACTOR_EXPANSION")
