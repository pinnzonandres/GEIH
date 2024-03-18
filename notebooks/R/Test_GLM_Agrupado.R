library(survey)

source("C:\\Users\\andre\\OneDrive\\Documentos\\Maestría\\Proyecto de Grado\\CPP Notebooks\\SMOTENC.R")

df <- read.csv("C:\\Users\\andre\\OneDrive\\Escritorio\\Proyecto de Grado\\result\\db_agrupado\\Enero_Agrupado.csv", encoding="LATIN1", sep=";")

data = subset(df, select = -c(DIRECTORIO,FACTOR_EXPANSION))

data$ACTIVIDAD_OCUPADA_ULTIMA_SEMANA = as.factor(data$ACTIVIDAD_OCUPADA_ULTIMA_SEMANA)
vars.numeric <- names(data)[sapply(data, is.numeric)]
data[, vars.numeric] <- scale(data[, vars.numeric])
vars.categoric <- names(data)[!names(data) %in% vars.numeric]

for (var in vars.categoric) {
  data[[var]] <- factor(data[[var]])
}

# División en entrenamiento y prueba (70% - 30%)
set.seed(123)
train_index <- sample(nrow(data), size = 0.7 * nrow(data), replace = FALSE)
train <- data[train_index, ]
test <- data[-train_index, ]

rownames(train) <- NULL
rownames(test) <- NULL

table(train$ACTIVIDAD_OCUPADA_ULTIMA_SEMANA)

resample <- SMOTE.ENC(train, "ACTIVIDAD_OCUPADA_ULTIMA_SEMANA", 0, 5, 123)

table(resample$ACTIVIDAD_OCUPADA_ULTIMA_SEMANA)

for (var in vars.numeric) {
  resample[[var]] <- as.numeric(resample[[var]])
}

modelo_logit <- glm(ACTIVIDAD_OCUPADA_ULTIMA_SEMANA ~ ., data = resample, family = "binomial")

summary(modelo_logit)

y_pred = predict.glm(modelo_logit, test, type="response")

y_pred_clas = as.factor(ifelse(y_pred > 0.5, 1, 0))


# Evaluación del Modelo
#install.packages("caret")
#install.packages("pROC")


# Cargar las bibliotecas
library(caret)
library(pROC)

# Calcula las métricas de evaluación
precision <- posPredValue(test$ACTIVIDAD_OCUPADA_ULTIMA_SEMANA, y_pred_clas)
recall <- sensitivity(test$ACTIVIDAD_OCUPADA_ULTIMA_SEMANA, y_pred_clas)
specificity(test$ACTIVIDAD_OCUPADA_ULTIMA_SEMANA, y_pred_clas)
