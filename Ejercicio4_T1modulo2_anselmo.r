# Problema 4 Tarea 1 módulo 2 Series de tiempo
# Librerías a utilizar 

library(portes)
library(vars)
library(forecast)

## Cargamos los datos de Clima.csv y Datos.csv respectivamente
datos_clima <- read.csv("C:/Users/Anselmo Daniel/Documents/Series de tiempo/Clima_F.csv")
datos <- read.csv("C:/Users/Anselmo Daniel/Documents/Series de tiempo/Datos.csv")

# Inciso a)
# Ajustar fechas
datos_clima$Periodo <- as.Date(paste("01", datos_clima$Periodo, sep = "-"), format = "%d-%b-%y")

# Creamos series temporales de Temperatura y Precipitación
clima_ts <- ts(datos_clima[, c("Temperatura", "Precipitacion")], 
               start = c(1985, 1), frequency = 12)

# Seleccionamos el número óptimo de retardos para el VAR de clima
lag_selection <- VARselect(clima_ts, lag.max = 12, type = "const")

# Ajustamos el modelo VAR(p) para las variables climáticas
modelo_VAR <- VAR(clima_ts, p = lag_selection$selection["AIC(n)"], type = "const")

# Prueba de causalidad de Granger para clima
granger_temperatura <- causality(modelo_VAR, cause = "Temperatura")
granger_precipitacion <- causality(modelo_VAR, cause = "Precipitacion")

# Imprimimos los resultados del inciso a)
#print(granger_temperatura$Granger)
#print(granger_precipitacion$Granger)

# Inciso b)
# Ajustar fechas a un formato adecuado para los datos económicos
datos$Fecha <- as.Date(paste0(datos$Fecha, "/01"), format = "%Y/%m/%d")

# Eliminar filas donde IPC o IGAE son NA
datos_limpios <- datos[!is.na(datos$IPC) & !is.na(datos$IGAE), ]

# Crear series temporales de Inflacion, IGAE, IPC y TC con los datos limpios
datos_ts_limpios <- ts(datos_limpios[, c("Inflacion", "IGAE", "IPC", "TC")], 
                       start = c(1980, 1), frequency = 12)

# Fusionar datos climáticos y económicos
merged_data <- cbind(clima_ts, datos_ts_limpios)

# Verificar si hay NA en los datos combinados
if (any(is.na(merged_data))) {
  # Eliminar filas con NA en los datos combinados
  merged_data_sin_na <- na.omit(merged_data)
} else {
  merged_data_sin_na <- merged_data
}

# Selección del número óptimo de retardos para los datos fusionados sin NA
lag_selection_merged <- VARselect(merged_data_sin_na, lag.max = 12, type = "const")

# Ajustar el modelo VAR(p) para los datos combinados
modelo_VAR_merged <- VAR(merged_data_sin_na, p = lag_selection_merged$selection["AIC(n)"], type = "const")



# Prueba de causalidad de Granger para las combinaciones entre clima y economía
# Usar los nombres correctos como aparecen en el modelo VAR
causalidad_temp_infla <- causality(modelo_VAR_merged, cause = "clima_ts.Temperatura")$Granger
causalidad_precip_ipc <- causality(modelo_VAR_merged, cause = "clima_ts.Precipitacion")$Granger

# Causales entre Temperatura y variables económicas
causalidad_temp_infla <- causality(modelo_VAR_merged, cause = "clima_ts.Temperatura")$Granger
causalidad_temp_ig <- causality(modelo_VAR_merged, cause = "clima_ts.Temperatura")$Granger
causalidad_temp_ipc <- causality(modelo_VAR_merged, cause = "clima_ts.Temperatura")$Granger
causalidad_temp_tc <- causality(modelo_VAR_merged, cause = "clima_ts.Temperatura")$Granger

# Causales entre Precipitación y variables económicas
causalidad_precip_infla <- causality(modelo_VAR_merged, cause = "clima_ts.Precipitacion")$Granger
causalidad_precip_ig <- causality(modelo_VAR_merged, cause = "clima_ts.Precipitacion")$Granger
causalidad_precip_ipc <- causality(modelo_VAR_merged, cause = "clima_ts.Precipitacion")$Granger
causalidad_precip_tc <- causality(modelo_VAR_merged, cause = "clima_ts.Precipitacion")$Granger

# Imprimir resultados
#print("Causalidad Temperatura a Inflación:")
#print(causalidad_temp_infla)

#print("Causalidad Temperatura a IGAE:")
#print(causalidad_temp_ig)

#print("Causalidad Temperatura a IPC:")
#print(causalidad_temp_ipc)

#print("Causalidad Temperatura a TC:")
#print(causalidad_temp_tc)

#print("Causalidad Precipitación a Inflación:")
#print(causalidad_precip_infla)

#print("Causalidad Precipitación a IGAE:")
#print(causalidad_precip_ig)

#print("Causalidad Precipitación a IPC:")
#print(causalidad_precip_ipc)

#print("Causalidad Precipitación a TC:")
#print(causalidad_precip_tc)