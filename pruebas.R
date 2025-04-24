#Instalación de las libreriás requeridas
library(ggplot2)
library(dplyr)
library(GGally)
library(xtable)
library(knitr)
library(forcats)
library(tidyr)
library(modeest)
library(kableExtra)

poblacion <- sample(0:1000, 1000, replace = TRUE)

library(readr)
info <- read_csv("datos.csv", col_types = cols(.default = col_integer()))
datos <- as.numeric(unlist(info))

prom <- mean(datos)

muestra1 <- sample(datos, size = 10, replace = FALSE)

muestra2 <- sample(datos, size = 10, replace = FALSE)
muestra3 <- sample(datos, size = 10, replace = FALSE)
muestra4 <- sample(datos, size = 10, replace = FALSE)
muestra5 <- sample(datos, size = 10, replace = FALSE)

prom1 <- mean(muestra1)
prom2 <- mean(muestra2)
prom3 <- mean(muestra3)
prom4 <- mean(muestra4)
prom5 <- mean(muestra5)

med <- median(datos)
mod <- mfv(datos)

ancho <- (max(datos) - min(datos))/10

cortes <- seq(min(datos), max(datos), by = ancho)
etiquetas <- character(length(cortes)-1)
etiquetas[1] <- paste0("[", round(cortes[1], 2), ", ", round(cortes[2], 2), "]")
if(length(cortes) > 2) {
  for(i in 2:(length(cortes)-1)) {
    etiquetas[i] <- paste0("(", round(cortes[i], 2), ", ", round(cortes[i+1], 2), "]")
  }
}
intervalos <- cut(datos, breaks = cortes, labels = etiquetas, include.lowest = TRUE, right = TRUE)

frec_abs <- table(intervalos)
frec_rel <- prop.table(frec_abs)
frec_abs_acum <- cumsum(frec_abs)
frec_rel_acum <- cumsum(frec_rel)

tabla_frecuencias_commercial <- data.frame(
  Intervalo = levels(intervalos),
  "F Absoluta" = as.numeric(frec_abs),
  "F Relativa" = round(as.numeric(frec_rel), 2),
  "F Abs Acum" = as.numeric(frec_abs_acum),
  "F Rel Acum" = round(as.numeric(frec_rel_acum), 2)
)

cortes <- seq(0, 1000, by = 100)
histograma <- ggplot(data.frame(datos = datos), aes(x = datos)) +
  geom_histogram(breaks = cortes, fill = "steelblue", color = "white", boundary = 0) +
  labs(x = "Intérvalos", y = "Frecuencia") +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, by = 100), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, NA) ,breaks = seq(0, 140, by = 20), expand = c(0, 0)) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

ggsave( filename = "figura1.png", plot = histograma,
        width = 6, height = 4, dpi = 150
)


histograma

info

tabla_frecuencias_commercial


set.seed(123) # Para reproducibilidad
medias_muestrales <- replicate(100, mean(sample(datos, size = 10, replace = FALSE)))

# Crear tabla de frecuencias
cortes_medias <- seq(300, 700, by = 50)
intervalos_medias <- cut(medias_muestrales, breaks = cortes_medias, include.lowest = TRUE)
frec_abs_medias <- table(intervalos_medias)

tabla_frec_medias <- data.frame(
  Intervalo = names(frec_abs_medias),
  Frecuencia = as.numeric(frec_abs_medias),
  Frecuencia_Relativa = round(as.numeric(prop.table(frec_abs_medias)), 2)
  
)

tabla_frec_medias


hist_medias <- ggplot(data.frame(medias = medias_muestrales), aes(x = medias)) +
  geom_histogram(aes(y = ..density..), bins = 8, fill = "steelblue", color = "white") +
  geom_density(color = "red", linewidth = 1) +
  labs(x = "Promedios muestrales", y = "Densidad", 
       title = "Distribución de promedios de muestras de tamaño 10") +
  theme_minimal()

ggsave("histograma_medias.png", plot = hist_medias, width = 6, height = 4, dpi = 150)
hist_medias



prom_poblacion <- mean(datos)
prom_medias <- mean(medias_muestrales)

comparacion_promedios <- data.frame(
  Población = prom_poblacion,
  Medias_Muestrales = prom_medias,
  Diferencia = prom_poblacion - prom_medias
)

