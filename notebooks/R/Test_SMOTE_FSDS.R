df <- read.csv("C:\\Users\\andre\\OneDrive\\Escritorio\\Proyecto de Grado\\result\\db_agrupado\\Enero_Agrupado.csv", encoding="LATIN1", sep=";")
df <- subset(df, select = -DIRECTORIO)

source("C:\\Users\\andre\\OneDrive\\Escritorio\\Proyecto de Grado\\notebooks\\R\\Balanceo.R")

sample.df =  df
sample.df$ACTIVIDAD_OCUPADA_ULTIMA_SEMANA = as.factor(sample.df$ACTIVIDAD_OCUPADA_ULTIMA_SEMANA)
vars.numeric <- names(sample.df)[sapply(sample.df, is.numeric)]

rownames(sample.df) <- NULL
table(sample.df$ACTIVIDAD_OCUPADA_ULTIMA_SEMANA)
sum(sample.df$FACTOR_EXPANSION)

# Valores Básicos
target = "ACTIVIDAD_OCUPADA_ULTIMA_SEMANA"
minority.value = 0
k = 5 
seed = 3

muestra <- SMOTE.ENC.FSDS(sample.df, target, minority.value, 'FACTOR_EXPANSION', k, vars.numeric, seed)

table(muestra$ACTIVIDAD_OCUPADA_ULTIMA_SEMANA)
sum(muestra$Adjusted_Weight)

write.csv(muestra, file = "C:\\Users\\andre\\OneDrive\\Escritorio\\Proyecto de Grado\\result\\db_agrupado\\Syntetic_Enero.csv",row.names = FALSE)
write.csv(sample.df, file = "C:\\Users\\andre\\OneDrive\\Escritorio\\Proyecto de Grado\\result\\db_agrupado\\Sample_Enero.csv", row.names = FALSE)



