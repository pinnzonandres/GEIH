# Balanceo de datos en la GEIH

Este repositorio contiene los datos y el código utilizados para el estudio sobre el balanceo de datos en el contexto de la GEIH.

## Datos

Los datos utilizados en el estudio se encuentran en el archivo [`FS_Enero.csv`](https://github.com/pinnzonandres/GEIH/blob/master/result/db_feature_selection/FS_Enero.csv) ubicado en la carpeta `result/db_feature_selection`. Este archivo contiene las características y la variable dependiente de las instancias del conjunto de datos.

## Código
El código que contiene la aplicación de los métodos de balanceo junto a los resultados obtenidos en el documento se encuentra en el archivo [`PruebaSMOTE_PSA.R`](https://github.com/pinnzonandres/GEIH/blob/master/notebooks/R/PruebaSMOTE_PSA.R). Este archivo contiene cada paso necesario para llegar a los resultados del documento.

El código para el estudio se encuentra en el archivo [`Balanceo.R`](https://github.com/pinnzonandres/GEIH/blob/master/notebooks/R/Balanceo.R) ubicado en la carpeta `notebooks\\R`. Este archivo contiene las implementaciones de los métodos de remuestreo, el entrenamiento y evaluación de los modelos de aprendizaje automático, y la visualización de los resultados.
