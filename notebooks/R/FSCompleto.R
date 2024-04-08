# Script con el feature selection utilizado dentro del conjunto GEIH de Enero

library(survey)
library(weights)
library(dplyr)


### Feature Selection

## Carga del Dataframe y definición de variables
data <- read.csv("C:/Users/andre/OneDrive/Escritorio/Proyecto de Grado/result/db_agrupado/Enero_Agrupado_Completo.csv", sep=";")
data <- subset(data, select = -DIRECTORIO)

data$ACTIVIDAD_OCUPADA_ULTIMA_SEMANA = as.factor(data$ACTIVIDAD_OCUPADA_ULTIMA_SEMANA)
vars.numeric <- names(data)[sapply(data, is.numeric)]
vars.numeric <- vars.numeric[vars.numeric != "FACTOR_EXPANSION"]

vars.categoric <- names(data)[!names(data) %in% vars.numeric]
vars.categoric <- vars.categoric[vars.categoric != "ACTIVIDAD_OCUPADA_ULTIMA_SEMANA"]
vars.categoric <- vars.categoric[vars.categoric != "FACTOR_EXPANSION"]

## Prueba Chi Cuadrado para variables nominales
chi_square_result = data.frame(t(sapply(vars.categoric, function(i){
  value = wtd.chi.sq(data[[i]], data$ACTIVIDAD_OCUPADA_ULTIMA_SEMANA, weight = data$FACTOR_EXPANSION)
  return(value)
})))

chi_square_result
# De acuerdo a lo visto desde la prueba de Chi Cuadrado, se van a descartar 
# aquellas variables que a un nivel de confianza del 95% no serían estadísticamente significativas

# Variables a descartar por el método Filter
to_drop_nominal = c( "SEXO", "ORIENTACION_SEXUAL", "GENERO",
                     "SERVICIOS_ENERGIA_ELECTRICA", "HOGAR_TIENE_CUENTA_CORRIENTE",
                     "HOGAR_TIENE_CUENTA_AHORROS", "HOGAR_TIENE_CDT", 
                     "HOGAR_TIENE_PRESTAMO_COMPRA_VIVIENDA",
                     "HOGAR_TIENE_PRESTAMO_COMPRA_VEHICULO",
                     "HOGAR_TIENE_PRESTAMO_LIBRE_INVERSION")


data$ACT_NUM = as.numeric(data$ACTIVIDAD_OCUPADA_ULTIMA_SEMANA)
corr_result <-wtd.cor(data[,vars.numeric], data$ACT_NUM ,weight = data$FACTOR_EXPANSION); corr_result
corr_result
to_drop_numeric = c(
  "NUMERO_HOGARES_VIVIENDA",
  "NUMERO_CUARTOS_VIVIENDA",
  "NUMERO_CUARTOS_DORMIR",
  "NUMERO_PERSONAS_HOGAR"
  )

vars.nominal.selected <-setdiff(vars.categoric, to_drop_nominal); vars.nominal.selected
vars.numeric.selected <- c("EDAD")  

vars.otras <- c("ACTIVIDAD_OCUPADA_ULTIMA_SEMANA", "FACTOR_EXPANSION")
vars.selected <- c(vars.numeric.selected, vars.nominal.selected, vars.otras); vars.selected

selected_data <- data[, vars.selected]

write.csv(
  selected_data, 
  file = "C:/Users/andre/OneDrive/Escritorio/Proyecto de Grado/result/db_feature_selection/FS_Enero_Completo.csv",
  row.names = FALSE
  )

