## Prueba del Algoritmo SMOTE ENC con los datos de Enero

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

# Definición de variables de tiempo
start_time <- Sys.time()
resample <- SMOTE.ENC(train, "ACTIVIDAD_OCUPADA_ULTIMA_SEMANA", 0, 5, 123)

## PROBLEMA DE BALANCEO POR MUESTREO
## X MODELAMIENTO POR MUESTREO
## METRICAS DE CALIDAD POR MUESTREO
end_time <- Sys.time()
table(df$ACTIVIDAD_OCUPADA_ULTIMA_SEMANA)

# Cálculo del tiempo de ejecución
tiempo_ejecucion <- end_time - start_time

# Impresión del tiempo
print(tiempo_ejecucion)

table(resample$ACTIVIDAD_OCUPADA_ULTIMA_SEMANA)

modelo_logit <- glm(ACTIVIDAD_OCUPADA_ULTIMA_SEMANA ~ ., data = resample, family = "binomial")

summary(modelo_logit)

step(modelo_logit)


modfinal = glm(formula = ACTIVIDAD_OCUPADA_ULTIMA_SEMANA ~ ZONA_TERRITORIAL + 
                 DPTO + EDAD + PARENTESCO_JEFE_DE_HOGAR + MADRE_RESIDE_HOGAR + 
                 PADRE_RESIDE_HOGAR + SE_CONSIDERA_CAMPESINO + COMUNIDAD_ES_CAMPESINA + 
                 ETNIA + ESTADO_CIVIL + AFILIADO_SALUD + ACTUALMENTE_ESTUDIA + 
                 MAYOR_NIVEL_EDUCATIVO + TIPO_VIVIENDA + ESTRATO_ENERGIA_ELECTRICA + 
                 SERVICIOS_GAS_NATURAL + SERVICIOS_ALCANTARILLADO + NUMERO_HOGARES_VIVIENDA + 
                 NUMERO_CUARTOS_VIVIENDA + NUMERO_CUARTOS_DORMIR + TIPO_SANITARIO + 
                 COMO_ELIMINA_BASURA + DONDE_OBTIENE_AGUA + DONDE_PREPARA_ALIMENTOS + 
                 TIPO_OCUPACION_VIVIENDA + HOGAR_TIENE_CUENTA_CORRIENTE + 
                 HOGAR_TIENE_CDT + HOGAR_TIENE_PRESTAMO_COMPRA_VIVIENDA + 
                 HOGAR_TIENE_PRESTAMO_COMPRA_VEHICULO + HOGAR_TIENE_PRESTAMO_LIBRE_INVERSION + 
                 HOGAR_TIENE_TARJETA_CREDITO + NUMERO_PERSONAS_HOGAR + DISCAPACIDAD, 
               family = "binomial", data = resample)

summary(modfinal)

predict(modfinal, test)

## Prueba con pesos de muestreo
pesos_muestreo <- df$FACTOR_EXPANSION
vivienda <- df$DIRECTORIO

# Diseño de Muestra Enero
design <- svydesign(id = ~vivienda, weights = ~pesos_muestreo, data = df)

design

svymodel = svyglm(formula = ACTIVIDAD_OCUPADA_ULTIMA_SEMANA ~ PARENTESCO_JEFE_DE_HOGAR + 
         MADRE_RESIDE_HOGAR + ESTADO_CIVIL + DPTO + ETNIA + ESTRATO_ENERGIA_ELECTRICA + 
         EDAD + NUMERO_CUARTOS_VIVIENDA + NUMERO_CUARTOS_DORMIR + 
         NUMERO_PERSONAS_HOGAR, design = design, family = "binomial")

summary(svymodel)


