# Instalar todos los paquetes necesarios
install.packages(c("terra", "readxl", "dplyr", "lubridate", "stringr", "ggplot2"))
# libraries
library(terra)        # for raster
library(readxl)       # for Excel
library(dplyr)        # for data wrangling
library(lubridate)    # for date manipulation
library(stringr)      # for regex
# === CONFIGURACIÓN ===
tiff_folder <- "C:/R/Snow"  # carpeta con .tif
estacion_file <- "C:/R/Snow/Snow.xlsx"
estacion_coord <- c(360507, 6013863) # UTM Este, Norte
# === FUNCIÓN: Extrae fecha desde nombre TIFF ===
extraer_fecha_tif <- function(nombre) {
  m <- str_match(nombre, "A(\\d{4})(\\d{3})")
  if (is.na(m[1,1])) return(NA)
  year <- as.integer(m[1,2])
  julian_day <- as.integer(m[1,3])
  as.Date(julian_day - 1, origin = paste0(year, "-01-01"))
}
# === LEER DATOS DE ESTACIÓN ===
df_estacion <- read_excel(estacion_file)
# 1. Normalizar nombres de columnas
names(df_estacion) <- tolower(names(df_estacion))
colnames(df_estacion)[1:2] <- c("fecha", "altura_cm")

# 2. Convertir la columna 'fecha' al tipo Date utilizando el formato dd/mm/yyyy
df_estacion <- df_estacion %>%
  mutate(fecha = as.Date(fecha, format = "%d/%m/%Y"))

# 3. Filtrar el data frame para quedarnos con fechas desde el 1 de mayo de 2024 hasta el 31 de octubre de 2024
df_estacion <- df_estacion %>%
  filter(fecha >= as.Date("2024-05-01") & fecha <= as.Date("2024-10-31"))
# === PROCESAR ARCHIVOS RASTER ===
# Obtener lista de archivos TIFF (asegurar que se lean)
tif_files <- list.files(tiff_folder, pattern = "\\.tif$", full.names = TRUE)

# Crear dataframe para almacenar valores del raster
df_raster <- data.frame(
  fecha = as.Date(character()),
  valor_raster = numeric(),
  stringsAsFactors = FALSE
)

# Bucle de extracción robusto
for (tif_file in tif_files) {
  # Extraer fecha del nombre del archivo
  fecha <- extraer_fecha_tif(basename(tif_file))
  
  # Cargar raster y extraer valor
  r <- rast(tif_file)
  extraccion <- terra::extract(r, matrix(estacion_coord, nrow = 1))  # Usar terra:: para precisión
  
  # Extraer valor usando el nombre de la banda (evitar índices numéricos)
  nombre_banda <- names(r)[1]  # Primera banda del raster
  valor <- extraccion[[nombre_banda]]
  
  # Registrar en el dataframe
  df_raster <- rbind(df_raster, data.frame(fecha = fecha, valor_raster = valor))
}

# === COMBINAR DATAFRAMES ===
df_completo <- df_estacion %>%
  left_join(df_raster, by = "fecha") %>%
  mutate(
    # Bandera de nieve del raster (1-100)
    raster_nieve = ifelse(valor_raster >= 1 & valor_raster <= 100, TRUE, FALSE),
    
    # Bandera de nieve de la estación (>0 cm)
    estacion_nieve = altura_cm > 0
  )

# Verificación de la unión
print("=== Estructura del dataframe combinado ===")
glimpse(df_completo)
# Guardar dataframe completo
write.csv(df_completo, "ResultadosComparacionNieve.csv", row.names = FALSE)



