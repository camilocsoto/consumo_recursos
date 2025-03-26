library(dplyr)
library(ggplot2)
library(lubridate)

data_f <- read.csv("./dataset/data_f.csv", stringsAsFactors = FALSE, sep = ";", check.names = FALSE)

# 2) Convertir FECHA TERMINO a tipo Date, ajustando el formato de día/mes/año si es necesario
data_f$`FECHA TERMINO` <- as.Date(data_f$`FECHA TERMINO`, format = "%d/%m/%Y")

# 3) Convertir CONSUMO a numérico (si en el CSV viene con coma como separador decimal)
data_f$CONSUMO <- as.numeric(gsub(",", ".", data_f$CONSUMO))

# 4) Agrupar y resumir el consumo de GAS por FECHA TERMINO y KEY Equipment
gas_data <- data_f %>%
  filter(CONCEPTO == "GAS") %>%
  group_by(`FECHA TERMINO`, `KEY Equipment`) %>%
  summarise(consumo_gas = sum(CONSUMO, na.rm = TRUE)) %>%
  ungroup()

# 5) Agrupar y resumir el consumo de Energía Eléctrica por FECHA TERMINO y KEY Equipment
elec_data <- data_f %>%
  filter(CONCEPTO == "Energía Eléctrica") %>%
  group_by(`FECHA TERMINO`, `KEY Equipment`) %>%
  summarise(consumo_elec = sum(CONSUMO, na.rm = TRUE)) %>%
  ungroup()

# 6) Combinar ambos data frames por FECHA TERMINO y KEY Equipment
combined_data <- left_join(gas_data, elec_data, by = c("FECHA TERMINO", "KEY Equipment"))

# 7) Calcular la correlación (opcional, pero útil)
corr_value <- cor(combined_data$consumo_gas, combined_data$consumo_elec, use = "complete.obs")
corr_value
# Imprime el valor de correlación en la consola

# 8) Crear el diagrama de dispersión con la línea de regresión
ggplot(combined_data, aes(x = consumo_gas, y = consumo_elec)) +
  geom_point(color = "brown", alpha = 0.7) +  # Puntos
  geom_smooth(method = "lm", color = "blue", fill = "lightblue") +  # Línea de regresión con banda de confianza
  labs(title = "Correlación entre Consumo de Gas y Energía Eléctrica",
       subtitle = paste("Coeficiente de correlación =", round(corr_value, 2)),
       x = "Consumo de Gas (m³)",
       y = "Consumo Eléctrico (kWh)") +
  theme_minimal()