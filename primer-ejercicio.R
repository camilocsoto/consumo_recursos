# ================================
# 1. Importación y limpieza de datos
# ================================

# Cargar librerías necesarias
library(dplyr)

# Importar el dataset (usar la ruta correcta)
data_f <- read.csv("./dataset/data_f.csv", stringsAsFactors = FALSE, check.names = FALSE, row.names = NULL, sep = ";")

# Eliminar filas donde "FECHA INICIO" es NA o cadena vacía
data_f <- data_f %>%
  filter(!(is.na(`FECHA INICIO`) | `FECHA INICIO` == ""))

# Eliminar filas donde "CONSUMO" es NA o cadena vacía
data_f <- data_f %>%
  filter(!is.na(CONSUMO) & CONSUMO != "")

# Eliminar filas donde "CONCEPTO" es NA o cadena vacía
data_f <- data_f %>%
  filter(!is.na(CONCEPTO) & CONCEPTO != "")

# Convertir formato numérico en "CONSUMO" (cambiar comas por puntos) y ajustar tipos de "ID" e "ID Unico"
data_f <- data_f %>%
  mutate(
    CONSUMO = as.numeric(gsub(",", ".", CONSUMO)),
    ID = as.character(ID),
    `ID Unico` = as.character(`ID Unico`)
  )

# Informe: calcular diferencia de filas entre el dataset original y el limpio
data_original <- read.csv("./dataset/data_f.csv", stringsAsFactors = FALSE, check.names = FALSE, row.names = NULL, sep = ";")
diferencia_filas <- nrow(data_original) - nrow(data_f)
cat("Cantidad de filas eliminadas: ", diferencia_filas, "\n")
cat("Cantidad de datos ajustados: ", nrow(data_f) * 3, "\n")
rm(data_original, diferencia_filas)

# ================================
# 2. Tablas de información y tipos de datos
# ================================

# Tabla: Unidades de cada concepto
concepto <- c("aire comprimido", "agua", "CDD", "Delta P", "Energia electrica",
              "energia refrigeracion", "GAS", "HDD", "Running Time", "VACIO", "VAPOR")
unidades <- c("[m3]", "[m3]", "[°C]", "[inHg] [mBAR]", "[kWh]", "[TNh]", "[m3]",
              "[°C]", "[hrs]", "[m3]", "[kg] [Tn]")

tabla_unidades <- data.frame(concepto, unidades)
print("Tabla de unidades de cada concepto:")
print(tabla_unidades)

# Tabla: Variables y tipos de datos
variables <- c("FECHA INICIO", "FECHA TERMINO", "CONCEPTO", "LINE", "KEY Equipment",
               "UNIDAD", "CONSUMO", "ID", "ID Unico", "ID FABRICACION")
tipos <- c("continua", "continua", "nominal", "nominal", "nominal", "nominal",
           "Continua", "Discreta", "Discreta", "nominal")
tabla_variables <- data.frame(variables, tipos)
print("\nTabla de variables y tipos de datos:")
print(tabla_variables)

# ================================
# 3. Cálculos estadísticos por concepto
# ================================

# 3.1. Media de consumo por Concepto
media_data  <- aggregate(CONSUMO ~ CONCEPTO, data = data_f, FUN = mean)
print("\nMedia de consumo por Concepto:")
print(media_data)

# 3.2. Mediana de consumo por Concepto
mediana_data <- aggregate(CONSUMO ~ CONCEPTO, data = data_f, FUN = median)
print("\nMediana de consumo por Concepto:")
print(mediana_data)

# 3.3. Moda de consumo por Concepto
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
moda_data <- aggregate(CONSUMO ~ CONCEPTO, data = data_f, FUN = getmode)
print("\nModa de consumo por Concepto:")
print(moda_data)

# 3.4. Desviación estándar de consumo por Concepto
sd_data  <- aggregate(CONSUMO ~ CONCEPTO, data = data_f, FUN = sd)
print("\nDesviación estándar de consumo por Concepto:")
print(sd_data)

# 3.5. Coeficiente de variación de consumo por Concepto
cv_data <- aggregate(CONSUMO ~ CONCEPTO, data = data_f, FUN = function(x) sd(x) / mean(x))
print("\nCoeficiente de variación de consumo por Concepto:")
print(cv_data)

# ================================
# 4. Resumen estadístico global por concepto
# ================================

tabla_resumen <- merge(mediana_data, media_data, by = "CONCEPTO", suffixes = c(".mediana", ".media")) %>%
  merge(moda_data, by = "CONCEPTO") %>%
  merge(sd_data, by = "CONCEPTO", suffixes = c("", ".sd")) %>%
  merge(cv_data, by = "CONCEPTO", suffixes = c("", ".cv")) %>%
  rename(Mediana = CONSUMO.mediana, Media = CONSUMO.media, Moda = CONSUMO, SD = CONSUMO.sd, CV = CONSUMO.cv)
print("\nResumen estadístico de consumo por concepto:")
print(tabla_resumen)