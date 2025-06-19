# Problema 4 Tarea 1 módulo 2  
#inciso c) Series de tiempo
# Librerías a utilizar 
library(portes)
library(vars)
library(forecast)

# Cargamos los datos de Clima.csv y Datos.csv respectivamente
datos_clima <- read.csv("C:/Users/Anselmo Daniel/Documents/Series de tiempo/Clima_F.csv")
datos <- read.csv("C:/Users/Anselmo Daniel/Documents/Series de tiempo/Datos.csv")

# Ajustar fechas
datos_clima$Periodo <- as.Date(paste("01", datos_clima$Periodo, sep = "-"), format = "%d-%b-%y")

# Creamos series temporales de Temperatura y Precipitación
clima_ts <- ts(datos_clima[, c("Temperatura", "Precipitacion")], 
               start = c(1985, 1), frequency = 12)

# Seleccionamos el número óptimo de retardos para el VAR de clima
lag_selection <- VARselect(clima_ts, lag.max = 12, type = "const")

# Ajustamos el modelo VAR(p) para las variables climáticas
modelo_VAR <- VAR(clima_ts, p = lag_selection$selection["AIC(n)"], type = "const")

# Dividir los datos en conjunto de entrenamiento y prueba
n <- length(clima_ts[, "Temperatura"])
train_size <- n - 24  


if (train_size <= 0) {
  stop("El tamaño del conjunto de entrenamiento no es válido. Asegúrate de que haya suficientes datos.")
}

# Crear conjunto de entrenamiento
train_data <- window(clima_ts, end = c(1985 + (train_size - 1) %/% 12, (train_size - 1) %% 12 + 1))

# Ajustar el modelo VAR a los datos de entrenamiento
modelo_VAR_train <- VAR(train_data, p = lag_selection$selection["AIC(n)"], type = "const")

# Predicción  para la temperatura
forecast_VAR <- predict(modelo_VAR_train, n.ahead = 24)
pred_temp_VAR <- forecast_VAR$fcst[["Temperatura"]][, 1]  


real_temp <- window(clima_ts[, "Temperatura"], start = c(1985 + train_size %/% 12, train_size %% 12 + 1))

# Verificamos que las longitudes coincidan
if (length(real_temp) != length(pred_temp_VAR)) {
  stop("Las longitudes de los datos reales y las predicciones no coinciden.")
}

# Evaluamos el modelo VAR
accuracy_VAR <- accuracy(pred_temp_VAR, real_temp)
print(accuracy_VAR)
# Ajustamos un modelo SARIMA a los datos de entrenamiento
modelo_SARIMA <- auto.arima(train_data[, "Temperatura"], seasonal = TRUE)

# Predicción  para SARIMA
forecast_SARIMA <- forecast(modelo_SARIMA, h = 24)
pred_temp_SARIMA <- forecast_SARIMA$mean


# Evaluar el modelo SARIMA
accuracy_SARIMA <- accuracy(pred_temp_SARIMA, real_temp)
print(accuracy_SARIMA)
# Compararamos los resultados
#print("Resultados del modelo VAR")
#print(accuracy_VAR)

#print("Resultados del modelo SARIMA")
#print(accuracy_SARIMA)



# Convertir real_temp y pred_temp_SARIMA a vectores continuos
real_temp_vector <- as.vector(real_temp)
pred_temp_SARIMA_vector <- as.vector(pred_temp_SARIMA)

# Verificar si las longitudes coinciden
if (length(pred_temp_SARIMA_vector) != length(real_temp_vector)) {
  stop("Las longitudes de los datos reales y las predicciones no coinciden.")
}


png("C:/Users/Anselmo Daniel/Documents/Series de tiempo/graf1t1mod1.png", width = 800, height = 600)
# Gráfico Conparativo
plot(real_temp_vector, type = "l", main = "Predicciones de Temperatura", xlab = "Tiempo", ylab = "Temperatura",
     col = "blue", lwd = 3, ylim = range(c(real_temp_vector, pred_temp_VAR, pred_temp_SARIMA_vector), na.rm = TRUE))

lines(pred_temp_VAR, col = "red", lty = 3)  
lines(pred_temp_SARIMA_vector, col = "green", lty = 3)  

legend("topleft", legend = c("Real", "VAR", "SARIMA"), col = c("blue", "red", "green"),
       lty = c(1, 2, 2), bty = "n")
dev.off()
