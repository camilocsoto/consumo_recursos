library(dplyr)
library(lubridate)
library(ggplot2)

# ================================
# 1.1. Histograma
# ================================
# Importar el dataset y limpiar datos:
data_f <- read.csv("./dataset/data_f.csv", stringsAsFactors = FALSE, check.names = FALSE, row.names = NULL, sep = ";")

# Convertir FECHA TERMINO a tipo Date
data_f$`FECHA TERMINO` <- as.Date(data_f$`FECHA TERMINO`, format = "%d/%m/%Y")

# Convertir CONSUMO a numérico si no lo has hecho antes
data_f$CONSUMO <- as.numeric(gsub(",", ".", data_f$CONSUMO))

# Extraer el mes de FECHA TERMINO
data_f$MES <- month(data_f$`FECHA TERMINO`, label = TRUE, abbr = TRUE)

# Filtrar solo AGUA y calcular el promedio mensual de consumo
datos_agua <- data_f %>%
  filter(CONCEPTO == "AGUA") %>%
  group_by(MES) %>%
  summarise(promedio_consumo = mean(CONSUMO, na.rm = TRUE)) %>%
  ungroup()

# Crear el gráfico de barras solo para AGUA
ggplot(datos_agua, aes(x = MES, y = promedio_consumo)) +
  geom_col(fill = "steelblue") +
  labs(title = "Consumo Mensual Promedio de Agua",
       x = "Mes",
       y = "Consumo Promedio (m^3)") +
  theme_minimal()


# ================================
# 1.2. Boxplot
# ================================


# Asegúrate de que FECHA TERMINO sea un Date válido
data_f$`FECHA TERMINO` <- as.Date(data_f$`FECHA TERMINO`, format = "%d/%m/%Y")

# Filtrar solo los registros donde el CONCEPTO es "AGUA"
agua_data <- data_f %>%
  filter(CONCEPTO == "AGUA")

# Crear la columna MES a partir de FECHA TERMINO
# 'label = TRUE' devuelve nombres de meses abreviados (Ej: Ene, Feb, ...)
agua_data$MES <- month(agua_data$`FECHA TERMINO`, label = TRUE, abbr = TRUE)

# Crear el boxplot agrupado por MES
ggplot(agua_data, aes(x = MES, y = CONSUMO)) +
  geom_boxplot(fill = "lightblue", color = "darkblue", 
               outlier.shape = 19, outlier.size = 2, outlier.alpha = 0.6) +
  scale_y_log10() +  # Si quieres ver mejor la distribución en caso de valores muy grandes
  labs(
    title = "Distribución Mensual del Consumo de Agua (Escala Logarítmica)",
    x = "Mes",
    y = "Consumo (log)"
  ) +
  theme_minimal()