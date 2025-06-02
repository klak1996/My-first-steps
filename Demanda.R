# Lista de librerías necesarias
paquetes <- c("readxl", "tidyr", "dplyr", "lubridate")

# Función para instalar y cargar cada paquete
for (p in paquetes) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, dependencies = TRUE)
  }
  library(p, character.only = TRUE)
}
# Cargar librerías necesarias
library(readxl)
library(tidyr)
library(dplyr)
library(lubridate)
# Leer el archivo Excel
datos <- read_excel("C:/R/Pattern/Demanda_Final.xlsx", sheet = "Hoja1")
# Renombrar la primera columna como "Fecha"
colnames(datos)[1] <- "Fecha"
# Mostrar la clase de la columna Fecha para verificar el formato
print(class(datos$Fecha))

# Convertir la columna Fecha a formato POSIXct según su tipo
datos <- datos %>%
  mutate(
    Fecha = if (is.numeric(Fecha)) {
      # Si es numérica, se asume formato Excel
      as.POSIXct(Fecha * 86400, origin = "1899-12-30", tz = "UTC")
    } else {
      # Si ya es de tipo fecha o carácter en el formato correcto
      as.POSIXct(Fecha)
    }
  )

# Convertir a formato largo y calcular fecha y hora
datos_largos <- datos %>%
  pivot_longer(
    cols = -Fecha,
    names_to = "Hora",
    values_to = "Demanda"
  ) %>%
  mutate(
    # Extraer el número de hora (removiendo "Hora ")
    Hora_num = as.numeric(gsub("Hora ", "", Hora)),
    
    # Sumar las horas adecuadas a la fecha
    FechaHora = Fecha + hours(Hora_num - 1)
  ) %>%
  # Seleccionar y ordenar columnas
  select(FechaHora, Demanda) %>%
  arrange(FechaHora)

# Verificar el resultado
head(datos_largos)
# Exportar a CSV la columna "Demanda" sin encabezado
write.table(
  datos_largos$Demanda, 
  file = "C:/R/Pattern/Demanda_Salida.csv", 
  sep = ",",
  row.names = FALSE, 
  col.names = FALSE, 
  quote = FALSE
)    
# Instalar y cargar ggplot2 si no lo tienes
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2", dependencies = TRUE)
}
library(ggplot2)

# Agrupar los datos por mes y sumar la demanda
datos_mes <- datos_largos %>%
  mutate(Mes = floor_date(FechaHora, "month")) %>%  # Extraer el primer día de cada mes
  group_by(Mes) %>%
  summarise(Demanda_mes = sum(Demanda, na.rm = TRUE)) %>%
  ungroup()

# Graficar la evolución de la demanda mensual
ggplot(datos_mes, aes(x = Mes, y = Demanda_mes)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 3) +
  geom_text(aes(label = round(Demanda_mes, 1)), 
            vjust = -0.5,       # Ajusta la posición vertical de las etiquetas
            size = 3) +
  scale_y_continuous(limits = c(16000, 20000)) +
  labs(
    x = "Mes",
    y = "Demanda Mensual kW",
    title = "Evolución de la Demanda Mensual"
  ) +
  theme_minimal()
# Crear el dataframe con la demanda promedio por hora y mes
datos_promedio <- datos_largos %>%
  mutate(
    Hora_dia = hour(FechaHora),                       # Extraer la hora del día (0-23)
    Mes = month(FechaHora, label = TRUE, abbr = TRUE)   # Convertir el mes a factor con nombre abreviado
  ) %>%
  group_by(Mes, Hora_dia) %>%
  summarise(Demanda_promedio = mean(Demanda, na.rm = TRUE)) %>%
  ungroup()
# Graficar la evolución de la demanda promedio por hora para cada mes
ggplot(datos_promedio, aes(x = Hora_dia, y = Demanda_promedio, color = Mes)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 0:23) +
  labs(
    x = "Hora del día",
    y = "Demanda Promedio",
    title = "Demanda Promedio por Hora y Mes",
    color = "Mes"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
# Análisis de frecuencia: calcular el percentil 90 de la demanda para cada hora y mes
datos_freq <- datos_largos %>%
  mutate(
    Hora_dia = hour(FechaHora),                       # Extraer la hora del día (0-23)
    Mes = month(FechaHora, label = TRUE, abbr = TRUE)   # Convertir el mes a factor con nombre abreviado
  ) %>%
  group_by(Mes, Hora_dia) %>%
  summarise(Demanda_90 = quantile(Demanda, probs = 0.90, na.rm = TRUE)) %>%
  ungroup()
# Graficar la demanda al percentil 90 para cada hora, con una curva por cada mes
ggplot(datos_freq, aes(x = Hora_dia, y = Demanda_90, color = Mes)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Análisis de Frecuencia: Demanda al 90% por Hora y Mes",
    x = "Hora del Día",
    y = "Demanda (Percentil 90)",
    color = "Mes"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# CALCULAR LA DEMANDA PROMEDIO DIARIA POR MES
# Primero se calcula el promedio diario a partir de 'datos_largos'
datos_diarios <- datos_largos %>%
  mutate(Fecha = as.Date(FechaHora)) %>%                # Convertir FechaHora a fecha (se ignora la hora)
  group_by(Fecha) %>%
  summarise(Demanda_diaria = mean(Demanda, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Mes = month(Fecha, label = TRUE, abbr = TRUE))   # Extraer el mes como factor con etiqueta abreviada

# Luego se calcula la demanda promedio diaria por mes promediando los promedios diarios de cada fecha
datos_promedio_diario <- datos_diarios %>%
  group_by(Mes) %>%
  summarise(Demanda_promedio_diaria = mean(Demanda_diaria, na.rm = TRUE)) %>%
  ungroup()
# Calcular límites dinámicos para el eje Y (margen del 5%)
min_y <- min(datos_promedio_diario$Demanda_promedio_diaria, na.rm = TRUE)
max_y <- max(datos_promedio_diario$Demanda_promedio_diaria, na.rm = TRUE)
margin <- (max_y - min_y) * 0.05

# GRAFICAR LA DEMANDA PROMEDIO DIARIA POR MES
ggplot(datos_promedio_diario, aes(x = Mes, y = Demanda_promedio_diaria, group = 1)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 3) +
  geom_text(aes(label = round(Demanda_promedio_diaria, 1)),
            vjust = -0.5, size = 3) +
  scale_y_continuous(limits = c(min_y - margin, max_y + margin)) +
  labs(
    x = "Mes",
    y = "Demanda Promedio Diaria (kW)",
    title = "Demanda Promedio Diaria por Mes"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# Agregar la columna 'Mes' a partir de 'FechaHora' del data frame 'datos_largos'
datos_largos <- datos_largos %>%
  mutate(Mes = month(FechaHora, label = TRUE, abbr = TRUE))

# Supongamos que 'datos_largos' ya tiene la columna 'Mes'
library(ggplot2)

# Boxplot de la Demanda Horaria por Mes con la Media marcada
ggplot(datos_largos, aes(x = Mes, y = Demanda)) +
  geom_boxplot(fill = "lightblue", color = "blue") + 
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red") + 
  labs(
    title = "Distribución Horaria de la Demanda por Mes (kW)",
    x = "Mes",
    y = "Demanda (kW)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x = 1.2, y = max(datos_largos$Demanda) * 0.95,
           label = "La caja representa el IQR,\nla línea es la mediana,\ny los puntos son outliers.\nEl punto rojo indica la media.",
           hjust = 0, size = 3, color = "black")
