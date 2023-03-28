# quejas_comprasporinternet
Estudio de quejas más frecuentes en compras por internet en el año 2022 en México
#Código Usado en R para el análisis de este proyecto 
#Instalamos los paquetes que vamos a utilizar 
install.packages("xlsx")
library(xlsx)

install.packages("ggthemes")
library(ggthemes)

install.packages("ggplot2")
library(ggplot2)

install.packages("DataExplorer")
library(DataExplorer)

#Obtener información sobre variables y observaciones
str(Quejas)

#Distribución de las quejas por tipo de reclamación
ggplot(Quejas, aes(x = "", fill = TIPO_RECLAMACION)) +
  geom_bar(width = 1, stat = "count") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Distribución de las quejas por tipo de reclamación")

#Identificar valores faltantes
plot_missing(Quejas)

#Limpieza de datos
Quejas_limpio <- na.omit(Quejas)
plot_missing(Quejas_limpio)

#Guardamos cambios en el proyecto
saveRDS(Quejas_limpio, file = "Quejas_limpio.rds")

#Obtener tablas de frecuencias por variable
tabla_valores_TIPO_RECLAMACION <- table(factor(Quejas_limpio$TIPO_RECLAMACION, levels = unique(Quejas_limpio$TIPO_RECLAMACION)))
 
tabla_ordenada <- sort(tabla_valores_TIPO_RECLAMACION, decreasing = TRUE)

tabla_ordenada

#Mostrar tablas en formato amigable
install.packages("knitr")
library(knitr)

tabla_valores_TIPO_RECLAMACION <- table(factor(Quejas_limpio$TIPO_RECLAMACION, levels = unique(Quejas_limpio$TIPO_RECLAMACION)))

tabla_ordenada <- sort(tabla_valores_TIPO_RECLAMACION, decreasing = TRUE)

kable(tabla_ordenada, caption = "Tabla de frecuencias de valores en la columna 'TIPO_RECLAMACION'")

#Guardamos cambios en el proyecto
saveRDS(tabla_ordenada, file = "tabla_ordenada.rds")

#Mostrar tabla de frecuencias de la variable MOTIVO_RECLAMACION
tabla_valores_MOTIVO_RECLAMACION <- table(factor(Quejas_limpio$MOTIVO_RECLAMACION, levels = unique(Quejas_limpio$MOTIVO_RECLAMACION)))
 
tabla_ordenada2 <- sort(tabla_valores_MOTIVO_RECLAMACION, decreasing = TRUE)

kable(tabla_ordenada2, caption = "Tabla de frecuencias de valores en la columna 'MOTIVO_RECLAMACION'")

#Obtener tabla de frecuencias de la variable NOMBRE_COMERCIAL de los primeros 25 resultados
tabla_valores_NOMBRE_COMERCIAL <- table(factor(Quejas_limpio$NOMBRE_COMERCIAL, levels = unique(Quejas_limpio$NOMBRE_COMERCIAL)))

tabla_ordenada <- sort(tabla_valores_NOMBRE_COMERCIAL, decreasing = TRUE)

kable(head(tabla_ordenada, 25), caption = "Tabla de frecuencias de valores en la columna 'NOMBRE_COMERCIAL'")

#Crear tabla con las columnas que nos interesan
tabla_Modalidad_Compra <- Quejas_limpio[, c("MODALIDAD COMPRA", "TIPO_RECLAMACION", "MOTIVO_RECLAMACION", "NOMBRE_COMERCIAL")]

# Crear el gráfico de barras de tabla Modalidad Compra
grafico_barras <- ggplot(tabla_Modalidad_Compra, aes(x = `MODALIDAD COMPRA`)) + 
  geom_bar(fill = "#69b3a2") + 
  scale_x_discrete(limits = c("Por internet", "En Establecimiento físico", "A domicilio", "Imposición del proveedor", "Por teléfono")) +
  labs(x = "Modalidad de compra", y = "Frecuencia") +
  ggtitle("Frecuencia de modalidades de compra")

# Mostrar el gráfico
grafico_barras

# Filtrar la tabla por la modalidad de compra "Por internet"

install.packages("dplyr")
library(dplyr)

tabla_por_internet <- filter(tabla_Modalidad_Compra, `MODALIDAD COMPRA` == "Por internet")

# Mostrar la tabla filtrada
tabla_por_internet

# Calcular la frecuencia por "TIPO_RECLAMACION"
 frec_por_tipo_reclamacion <- table(tabla_por_internet$TIPO_RECLAMACION)

# Convertir los resultados en un data frame
tabla_frec_por_tipo_reclamacion <- data.frame(
    TIPO_RECLAMACION = names(frec_por_tipo_reclamacion),
    FRECUENCIA = as.numeric(frec_por_tipo_reclamacion)
)

 
# Ordenar la tabla por "FRECUENCIA" de manera descendente
tabla_frec_por_tipo_reclamacion <- tabla_frec_por_tipo_reclamacion[order(-tabla_frec_por_tipo_reclamacion$FRECUENCIA), ]

# Mostrar la tabla en formato markdown
kable(tabla_frec_por_tipo_reclamacion, format = "markdown", align = "c",
      caption = "Frecuencia por TIPO_RECLAMACION de la tabla_por_internet")

# Cargar la biblioteca dplyr
library(dplyr)

# Agrupar la tabla por NOMBRE_COMERCIAL y contar el número de quejas para cada uno, ordenado por frecuencia descendente
tabla_frecuencia <- tabla_por_internet %>%
  group_by(NOMBRE_COMERCIAL) %>%
  summarise(frecuencia = n()) %>%
  arrange(desc(frecuencia))

# Imprimir todas las filas de la tabla
print(tabla_frecuencia, n = nrow(tabla_frecuencia))

#Obtención de datos relevantes de Suburbia
datos_suburbia <- tabla_Modalidad_Compra[tabla_Modalidad_Compra$NOMBRE_COMERCIAL == "SUBURBIA", ]

library(dplyr)

datos_suburbia_ordenados <- datos_suburbia %>%
  group_by(TIPO_RECLAMACION) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia))

# Crea un vector de colores para el gráfico de pastel
colores <- c("#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#e41a1c")

# Hacer el gráfico de pastel
pie(datos_suburbia_ordenados$Frecuencia,
    labels = datos_suburbia_ordenados$TIPO_RECLAMACION,
    col = colores,
    main = "Tipos de reclamación en Suburbia")

datos_suburbia_ordenados2 <- datos_suburbia %>%
  group_by(MOTIVO_RECLAMACION) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia))


datos_suburbia_ordenados2

#Obtención de datos relevantes de WalMart
datos_Walmart <- tabla_Modalidad_Compra[tabla_Modalidad_Compra$NOMBRE_COMERCIAL == "WALMART", ]


datos_Walmart_ordenados <- datos_Walmart %>%
  group_by(TIPO_RECLAMACION) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia))

datos_Walmart_ordenados

# Crear un vector de colores para el gráfico de pastel
colores <- c("#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#e41a1c")

# Hacer el gráfico de pastel
pie(datos_Walmart_ordenados$Frecuencia,
    labels = datos_suburbia_ordenados$TIPO_RECLAMACION,
    col = colores,
    main = "Tipos de reclamación en Walmart")

datos_Walmart_ordenados2 <- datos_Walmart %>%
  group_by(MOTIVO_RECLAMACION) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia))


datos_Walmart_ordenados2

#Obtención de datos relevantes de Aeroméxico
datos_Aeromexico <- tabla_Modalidad_Compra[tabla_Modalidad_Compra$NOMBRE_COMERCIAL == "AEROMÉXICO", ]

datos_Aeromexico_ordenados <- datos_Aeromexico %>%
  group_by(TIPO_RECLAMACION) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia))

datos_Aeromexico_ordenados

# Crea un vector de colores para el gráfico de pastel
colores <- c("#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#e41a1c")

# Hace el gráfico de pastel
pie(datos_Aeromexico_ordenados$Frecuencia,
    labels = datos_Aeromexico_ordenados$TIPO_RECLAMACION,
    col = colores,
    main = "Tipos de reclamación en Aeroméxico")

datos_Aeromexico_ordenados2 <- datos_Aeromexico %>%
  group_by(MOTIVO_RECLAMACION) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia))


datos_Aeromexico_ordenados2

#Obtención de datos relevantes de Mercado Libre
datos_MercadoLibre <- tabla_Modalidad_Compra[tabla_Modalidad_Compra$NOMBRE_COMERCIAL == "MERCADOLIBRE", ]

datos_MercadoLibre_ordenados <- datos_MercadoLibre %>%
  group_by(TIPO_RECLAMACION) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia))

datos_MercadoLibre_ordenados

# Crear un vector de colores para el gráfico de pastel
colores <- c("#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#e41a1c")

# Hacer el gráfico de pastel
pie(datos_MercadoLibre_ordenados$Frecuencia,
    labels = datos_MercadoLibre_ordenados$TIPO_RECLAMACION,
    col = colores,
    main = "Tipos de reclamación en Mercado Libre")

datos_MercadoLibre_ordenados2 <- datos_MercadoLibre %>%
  group_by(MOTIVO_RECLAMACION) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia))

datos_MercadoLibre_ordenados2

#Obtención de datos relevantes de Brandescard
datos_Bradescard <- tabla_Modalidad_Compra[tabla_Modalidad_Compra$NOMBRE_COMERCIAL == "BRADESCARD", ]

datos_Bradescard_ordenados <- datos_Bradescard %>%
  group_by(TIPO_RECLAMACION) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia))

# Crear un vector de colores para el gráfico de pastel
colores <- c("#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#e41a1c")

# Hacer el gráfico de pastel
pie(datos_Bradescard_ordenados$Frecuencia,
    labels = datos_Bradescard_ordenados$TIPO_RECLAMACION,
    col = colores,
    main = "Tipos de reclamación en Bradescard")

datos_Bradescard_ordenados2 <- datos_Bradescard %>%
  group_by(MOTIVO_RECLAMACION) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia))

datos_Bradescard_ordenados2

#Obtención de datos relevantes de VivaAerobus
datos_VivaAerobus <- tabla_Modalidad_Compra[tabla_Modalidad_Compra$NOMBRE_COMERCIAL == "VIVA AEROBUS", ]

datos_VivaAerobus_ordenados <- datos_VivaAerobus %>%
  group_by(TIPO_RECLAMACION) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia))

datos_VivaAerobus_ordenados

# Crear un vector de colores para el gráfico de pastel
colores <- c("#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#e41a1c")

# Hacer el gráfico de pastel
pie(datos_VivaAerobus_ordenados$Frecuencia,
    labels = datos_VivaAerobus_ordenados$TIPO_RECLAMACION,
    col = colores,
    main = "Tipos de reclamación en Viva Aerobus")

datos_VivaAerobus_ordenados2 <- datos_VivaAerobus %>%
  group_by(MOTIVO_RECLAMACION) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia))

datos_VivaAerobus_ordenados2

#Obtención de datos relevantes de Amazon.com.mx
datos_Amazon <- tabla_Modalidad_Compra[tabla_Modalidad_Compra$NOMBRE_COMERCIAL == "AMAZON.COM.MX", ]

datos_Amazon_ordenados <- datos_Amazon %>%
group_by(TIPO_RECLAMACION) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia))

datos_Amazon_ordenados

# Crear un vector de colores para el gráfico de pastel
colores <- c("#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#e41a1c")

# Hacer el gráfico de pastel
pie(datos_Amazon_ordenados$Frecuencia,
    labels = datos_Amazon_ordenados$TIPO_RECLAMACION,
    col = colores,
    main = "Tipos de reclamación en AMAZON.COM.MX")

datos_Amazon_ordenados2 <- datos_Amazon %>%
  group_by(MOTIVO_RECLAMACION) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia))

datos_Amazon_ordenados2

#Obtención de datos del Palacio de Hierro
datos_PalacioHierro <- tabla_Modalidad_Compra[tabla_Modalidad_Compra$NOMBRE_COMERCIAL == "EL PALACIO DE HIERRO", ]

datos_PalacioHierro_ordenados <- datos_PalacioHierro %>%
group_by(TIPO_RECLAMACION) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia))

datos_PalacioHierro_ordenados

# Crea un vector de colores para el gráfico de pastel
colores <- c("#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#e41a1c")

# Hace el gráfico de pastel
pie(datos_PalacioHierro_ordenados$Frecuencia,
    labels = datos_PalacioHierro_ordenados$TIPO_RECLAMACION,
    col = colores,
    main = "Tipos de reclamación en PALACIO DE HIERRO")

datos_PalacioHierro_ordenados2 <- datos_PalacioHierro %>%
  group_by(MOTIVO_RECLAMACION) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia))

datos_PalacioHierro_ordenados2

#Obtención de datos de Sears
datos_Sears <- tabla_Modalidad_Compra[tabla_Modalidad_Compra$NOMBRE_COMERCIAL == "SEARS", ]

datos_Sears_ordenados <- datos_Sears %>%
group_by(TIPO_RECLAMACION) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia))

datos_Sears_ordenados

# Crea un vector de colores para el gráfico de pastel
colores <- c("#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#e41a1c")

# Hace el gráfico de pastel
pie(datos_Sears_ordenados$Frecuencia,
    labels = datos_Sears_ordenados$TIPO_RECLAMACION,
    col = colores,
    main = "Tipos de reclamación en SEARS")
datos_Sears_ordenados2 <- datos_Sears %>%
  group_by(MOTIVO_RECLAMACION) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia))

datos_Sears_ordenados2

#Obtención de datos de Tarjetas del Futuro, SAPI de CV
datos_DelFuturo <- tabla_Modalidad_Compra[tabla_Modalidad_Compra$NOMBRE_COMERCIAL == "TARJETAS DEL FUTURO, SAPI DE CV", ]

datos_DelFuturo_ordenados <- datos_DelFuturo %>%
group_by(TIPO_RECLAMACION) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia))

# Crea un vector de colores para el gráfico de pastel
colores <- c("#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#e41a1c")

# Hace el gráfico de pastel
pie(datos_DelFuturo_ordenados$Frecuencia,
    labels = datos_DelFuturo_ordenados$TIPO_RECLAMACION,
    col = colores,
    main = "Tipos de reclamación en TARJETAS DEL FUTURO, SAPI DE CV")

datos_DelFuturo_ordenados2 <- datos_Sears %>%
  group_by(MOTIVO_RECLAMACION) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia))

datos_DelFuturo_ordenados2


