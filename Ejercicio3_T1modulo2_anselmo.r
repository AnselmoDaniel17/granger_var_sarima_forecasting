#Ejercicio 3 Tarea 1 Módulo 2 Series de tiempo 
# Instalar y cargar el paquete necesario
if (!requireNamespace("vars", quietly = TRUE)) {
  install.packages("vars")
}
library(vars)

# Establecer la semilla para la reproducibilidad
#set.seed(123)

# Simular datos
n <- 100  # Número de observaciones
time <- 1:n

# Generar dos series temporales correlacionadas
x1 <- cumsum(rnorm(n))  # Serie 1 (aleatoria)
x2 <- 0.5 * x1 + rnorm(n)  # Serie 2 (correlacionada con x1)

# Crear un data frame
data <- data.frame(time = time, x1 = x1, x2 = x2)

# Estimar modelos VAR para diferentes órdenes y calcular FPE
max_order <- 5  # Máximo orden a probar
fpe_values <- numeric(max_order)

for (m in 1:max_order) {
  # Ajustar el modelo VAR
  var_model <- VAR(data[, c("x1", "x2")], p = m)
  
  # Calcular la matriz de covarianza residual
  sigma_u <- summary(var_model)$covres
  
  # Calcular el número de parámetros del modelo
  K <- ncol(data)  # Número de series
  num_params <- m * K^2 + K  # Parámetros del modelo VAR(m)

  # Calcular el FPE
  T <- nrow(data)  # Tamaño de la muestra
  fpe_values[m] <- det(sigma_u) * (T + num_params) / (T - num_params - 1)
}

# Resultados
fpe_values
best_order <- which.min(fpe_values)
cat("Valores de FPE para diferentes órdenes:\n", fpe_values, "\n")
cat("El mejor orden según el criterio FPE es:", best_order, "\n")




# Graficar los valores de FPE
png("C:/Users/Anselmo Daniel/Documents/Series de tiempo/graf2t1mod1.png", width = 800, height = 600)
plot(1:max_order, fpe_values, type = "b", pch = 19, 
     xlab = "Orden del modelo VAR", 
     ylab = "FPE", 
     main = "Criterio de Error de Predicción Final (FPE)")
abline(v = best_order, col = "red", lty = 2)
text(best_order, fpe_values[best_order], labels = best_order, pos = 4)
dev.off()
