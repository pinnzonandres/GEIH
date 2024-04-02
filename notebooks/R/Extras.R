library(survey)
library(mlbench)
library(caret)
library(pROC)
library(Metrics)

survey_model <- function(train, test, str_wt_name) {
    ids <- seq(1, nrow(train))
    wts <- train[[str_wt_name]]

    design <- svydesign(ids = ids, data = train, weights = wts)

    #modelo <- svyglm(formula = ACTIVIDAD_OCUPADA_ULTIMA_SEMANA ~  DPTO + SEXO + EDAD + 
    #                    PARENTESCO_JEFE_DE_HOGAR + MADRE_RESIDE_HOGAR + ETNIA + ESTADO_CIVIL +  
    #                    MAYOR_NIVEL_EDUCATIVO + ORIENTACION_SEXUAL + GENERO + ESTRATO_ENERGIA_ELECTRICA + 
    #                    NUMERO_CUARTOS_VIVIENDA + NUMERO_CUARTOS_DORMIR + TIPO_OCUPACION_VIVIENDA + NUMERO_PERSONAS_HOGAR,
    #                  design = design,
    #                  family = quasibinomial())

    modelo <- svyglm(formula = ACTIVIDAD_OCUPADA_ULTIMA_SEMANA ~ ZONA_TERRITORIAL + DPTO + SEXO + EDAD + 
                        PARENTESCO_JEFE_DE_HOGAR + MADRE_RESIDE_HOGAR + PADRE_RESIDE_HOGAR + SE_CONSIDERA_CAMPESINO +              
                        COMUNIDAD_ES_CAMPESINA + ETNIA + ESTADO_CIVIL + AFILIADO_SALUD + LEER_ESCRIBIR + 
                        ACTUALMENTE_ESTUDIA + MAYOR_NIVEL_EDUCATIVO + ORIENTACION_SEXUAL + GENERO + 
                        TIPO_VIVIENDA + SERVICIOS_ENERGIA_ELECTRICA + ESTRATO_ENERGIA_ELECTRICA + SERVICIOS_GAS_NATURAL + 
                        SERVICIOS_ALCANTARILLADO + SERVICIOS_RECOLECCION_BASURAS +SERVICIOS_ACUEDUCTO+
                        NUMERO_HOGARES_VIVIENDA +NUMERO_CUARTOS_VIVIENDA +NUMERO_CUARTOS_DORMIR +               
                        TIPO_SANITARIO+COMO_ELIMINA_BASURA +DONDE_OBTIENE_AGUA +DONDE_PREPARA_ALIMENTOS +             
                        TIPO_OCUPACION_VIVIENDA+HOGAR_TIENE_CUENTA_CORRIENTE +HOGAR_TIENE_CUENTA_AHORROS +HOGAR_TIENE_CDT +                     
                        HOGAR_TIENE_PRESTAMO_COMPRA_VIVIENDA+HOGAR_TIENE_PRESTAMO_COMPRA_VEHICULO +HOGAR_TIENE_PRESTAMO_LIBRE_INVERSION +
                        HOGAR_TIENE_TARJETA_CREDITO +NUMERO_PERSONAS_HOGAR+DISCAPACIDAD,
                      design = design,
                      family = quasibinomial())
    
    y_pred <- ifelse(predict.glm(modelo, newdata = test, type = "response") > 0.5, 1, 0)

    # Calculo de Métricas
    #matriz de confusión
    cm <- confusionMatrix(as.factor(y_pred), as.factor(test$ACTIVIDAD_OCUPADA_ULTIMA_SEMANA))
    print(cm)

    prec <- Metrics::precision(actual = test$ACTIVIDAD_OCUPADA_ULTIMA_SEMANA,
                   predicted = y_pred)
    
    print("Precision")
    print(prec)
    reca <- Metrics::recall(actual = test$ACTIVIDAD_OCUPADA_ULTIMA_SEMANA,
                   predicted = y_pred) 

    print("Recall")
    print(reca)
    f1 <- (2*prec*reca)/(prec+reca)
    
    print("F1_SCORE")
    print(f1)

    print("AUC")
    roc_object <- pROC::roc(test$ACTIVIDAD_OCUPADA_ULTIMA_SEMANA,y_pred)
    auc_value <- pROC::auc(roc_object)
    print(auc_value)

    return(modelo)
} 