# ============================================================
# R para análisis de datos: crecimiento de peces en cultivo
# Versión autocontenida: NO necesita ningún archivo CSV
# ============================================================

# --- 1. Datos (simulados dentro de R) -----------------------
set.seed(42)

especies   <- c("Tilapia", "Trucha", "Bagre")
temp_base  <- c(28, 18, 26)      # temperatura típica por especie (°C)
k_especie  <- c(0.9, 1.1, 0.8)   # factor de crecimiento por especie
dietas     <- c("A", "B", "C")
f_dieta    <- c(1.00, 1.12, 0.95) # factor de crecimiento por dieta

grid <- expand.grid(rep = 1:20, dieta = 1:3, especie = 1:3)
n <- nrow(grid)  # 180 registros

dias           <- sample(c(30, 60, 90), n, replace = TRUE)
temperatura_c  <- round(rnorm(n, mean = temp_base[grid$especie], sd = 1.2), 1)
peso_inicial_g <- round(runif(n, 8, 15), 1)
ganancia       <- peso_inicial_g * 0.02 * dias *
  k_especie[grid$especie] * f_dieta[grid$dieta] *
  (1 + rnorm(n, 0, 0.12))
peso_final_g   <- round(peso_inicial_g + ganancia, 1)
oxigeno_mgl    <- round(rnorm(n, mean = 6.5, sd = 0.7), 2)

datos <- data.frame(
  id             = 1:n,
  especie        = factor(especies[grid$especie], levels = especies),
  dieta          = factor(dietas[grid$dieta]),
  dias           = dias,
  temperatura_c  = temperatura_c,
  oxigeno_mgl    = oxigeno_mgl,
  peso_inicial_g = peso_inicial_g,
  peso_final_g   = peso_final_g
)

# (Opcional) guardar la base como CSV en tu carpeta de trabajo:
# write.csv(datos, "acuacultura.csv", row.names = FALSE)

head(datos)
str(datos)
summary(datos)

# --- 2. Funciones sencillas --------------------------------

# Función 1: media y desviación estándar
resumen_basico <- function(x) {
  c(media = mean(x, na.rm = TRUE),
    desv_est = sd(x, na.rm = TRUE))
}

# Función 2: coeficiente de variación (%)
coef_variacion <- function(x) {
  sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100
}

# Función 3: tasa de crecimiento diario (g/día)
tasa_crecimiento <- function(peso_inicial, peso_final, dias) {
  (peso_final - peso_inicial) / dias
}

# Función 4: histograma de cualquier variable numérica
hacer_histograma <- function(x, titulo, etiqueta_x, color = "steelblue") {
  hist(x, breaks = 15, col = color, border = "white",
       main = titulo, xlab = etiqueta_x, ylab = "Frecuencia")
}

# --- 3. Uso de las funciones -------------------------------
datos$tasa_g_dia <- tasa_crecimiento(datos$peso_inicial_g,
                                     datos$peso_final_g,
                                     datos$dias)

resumen_basico(datos$peso_final_g)
coef_variacion(datos$peso_final_g)

# Resumen de la tasa de crecimiento por especie y por dieta
sapply(split(datos$tasa_g_dia, datos$especie), resumen_basico)
sapply(split(datos$tasa_g_dia, datos$dieta), resumen_basico)

# --- 4. Gráficos -------------------------------------------

# 4.1 Histograma del peso final (usa la función 4)
hacer_histograma(datos$peso_final_g,
                 titulo = "Distribución del peso final",
                 etiqueta_x = "Peso final (g)")

# 4.2 Boxplot de la tasa de crecimiento por especie
boxplot(tasa_g_dia ~ especie, data = datos,
        col = c("#4C9F70", "#4C78A8", "#E3A33B"),
        main = "Tasa de crecimiento por especie",
        xlab = "Especie", ylab = "Tasa de crecimiento (g/día)")

# 4.3 Boxplot de la tasa de crecimiento por dieta
boxplot(tasa_g_dia ~ dieta, data = datos,
        col = "lightblue",
        main = "Tasa de crecimiento por dieta",
        xlab = "Dieta", ylab = "Tasa de crecimiento (g/día)")

# 4.4 Dispersión: temperatura vs. tasa de crecimiento
colores <- c("#4C9F70", "#4C78A8", "#E3A33B")[as.integer(datos$especie)]
plot(datos$temperatura_c, datos$tasa_g_dia,
     col = colores, pch = 19,
     main = "Temperatura vs. tasa de crecimiento",
     xlab = "Temperatura (°C)", ylab = "Tasa de crecimiento (g/día)")
legend("topleft", legend = levels(datos$especie),
       col = c("#4C9F70", "#4C78A8", "#E3A33B"), pch = 19, bty = "n")

# 4.5 Barras: tasa promedio por especie y dieta
promedios <- tapply(datos$tasa_g_dia, list(datos$dieta, datos$especie), mean)
barplot(promedios, beside = TRUE,
        col = c("#9ecae1", "#6baed6", "#2171b5"),
        main = "Tasa de crecimiento promedio por especie y dieta",
        xlab = "Especie", ylab = "Tasa media (g/día)")
legend("topright", title = "Dieta", legend = rownames(promedios),
       fill = c("#9ecae1", "#6baed6", "#2171b5"), bty = "n")

