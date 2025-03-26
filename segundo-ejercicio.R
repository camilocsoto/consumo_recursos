# Cargar librerías necesarias
library(ggplot2)
library(dplyr)

# Cargar el dataset
data_f <- read.csv("./dataset/data_f.csv", stringsAsFactors = FALSE, check.names = FALSE, row.names = NULL, sep = ";")

# Convertir la columna "CONSUMO" en numérica (removiendo comas y cambiando a punto decimal si es necesario)
data_f$CONSUMO <- as.numeric(gsub(",", ".", data_f$CONSUMO))

# Filtrar los principales recursos
recursos_principales <- c("GAS", "AGUA", "Energía Eléctrica")
datos_principales <- data_f %>% filter(CONCEPTO %in% recursos_principales)

# Crear el histograma con densidad
ggplot(datos_principales, aes(x = CONSUMO, fill = CONCEPTO)) +
  geom_histogram(aes(y = ..density..), bins = 50, color = "black", alpha = 0.4, position = "identity") +
  geom_density(alpha = 0.3) +
  facet_wrap(~CONCEPTO, scales = "free") +  # Crea subgráficos por concepto
  scale_fill_manual(values = c("blue", "green", "red")) +  # Colores para cada recurso
  geom_vline(aes(xintercept = median(CONSUMO, na.rm = TRUE)), 
             color = "blue", linetype = "dashed", size = 1) +  # Línea punteada en la mediana
  labs(title = "Histograma con Densidad por Tipo de Consumo",
       x = "Consumo",
       y = "Densidad",
       fill = "CONCEPTO") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 12))  # Personaliza los títulos de los subgráficos