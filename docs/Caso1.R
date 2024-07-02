install.packages("ggplot2")
install.packages("dplyr")
install.packages("reshape")

#Librerías
library(ggplot2)
library(dplyr)
library(reshape)

setwd('/Users/davidbacca/OneDrive - Politécnico Grancolombiano/2024-II/Diplomado Narrativa ICD/M No Supervisados/Casos/Caso1_EDA/')

#Cargar conjunto de datos
accidentes <- read.csv('accidents.csv', header = T, sep = ';')
head(accidentes)

#filas y columnas
nrow(accidentes)
ncol(accidentes)

#dar formato a las fechas
class(accidentes$DATE)
accidentes$DATE <- as.Date(accidentes$DATE, '%m/%d/%Y')

#extraer el mes del año
accidentes$m_y <- strftime(accidentes$DATE, '%y-%m')

#Accidentes por mes
accidentes_mes <- as.data.frame(accidentes %>% count(m_y))

ggplot(accidentes_mes, aes(x = m_y, y = n, group = 1)) +
  geom_line()+
  xlab("Mes") + 
  ylab("No. de accidentes") +
  scale_y_continuous(limits = c(0, NA))

#Accidentes por hora
accidentes$TIME <- strptime(accidentes$TIME, '%H:%M')
accidentes$hora <- strftime(accidentes$TIME, '%H')

accidentes_hora <- as.data.frame(accidentes %>% count(hora))

#Gráfico de línea
ggplot(accidentes_hora, aes(x = hora, y = n, group = 1)) +
  geom_line()+
  xlab("Hora") + 
  ylab("No. de accidentes") +
  scale_y_continuous(limits = c(0, NA))

#Gráfico de barras
ggplot(accidentes_hora, aes(x = hora, y = n, group = 1)) +
  geom_bar(stat = "identity")+
  xlab("Hora") + 
  ylab("No. de accidentes")

##Accidentes en días de la semana##
Sys.setlocale("LC_TIME", "es_ES.UTF-8")  # En sistemas Unix/Linux
#Sys.setlocale("LC_TIME", "Spanish")    # En sistemas Windows

accidentes$dia <- weekdays(accidentes$DATE)

accidentes_dia <- as.data.frame(accidentes %>% count(dia))
accidentes_dia$dia <- factor(accidentes_dia$dia)

levels(accidentes_dia$dia) <- c("viernes", "lunes", "sábado", "domingo", "jueves", "martes", "miércoles")

dias_ordenados <- c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")

accidentes_dia$dia <- factor(accidentes_dia$dia, levels = dias_ordenados)
accidentes_dia <- accidentes_dia %>% arrange(dia)

ggplot(accidentes_dia, aes(x = dia, y = n, group = 1)) +
  geom_bar(stat = "identity")+
  xlab("Día de la semana") + 
  ylab("No. de accidentes")

#accidentes por vecindario
vec <- as.data.frame(accidentes %>% count(BOROUGH))
ggplot(vec, aes(x = BOROUGH, y = n, group = 1)) +
  geom_bar(stat = "identity")+
  xlab("Vecindario") + 
  ylab("No. de accidentes")

#Información de los vecindarios
borough_data <- read.csv(file = 'borough_data.csv', header = T)

borough_data$borough <- toupper(borough_data$borough)
borough_data$borough[borough_data$borough == 'THE BRONX'] <- "BRONX"
names(borough_data)[1] <- 'BOROUGH'
#left_join(x = vec, y = borough_data, by = c('BOROUGH' = 'borough'))
vec <- left_join(x = vec, y = borough_data, by = 'BOROUGH')
vec <- vec %>% mutate(accidentes_por_area = n / area)

ggplot(vec, aes(x = BOROUGH, y = accidentes_por_area, group = 1)) +
  geom_bar(stat = "identity")+
  xlab("Vecindario") + 
  ylab("No. de accidentes por unidad de área")

#accidentes por hora y por vecindario

accidentes_hora_borough <- accidentes %>% count(BOROUGH, hora)

ggplot(accidentes_hora_borough, aes(x = hora, y = n, colour = factor(BOROUGH))) +
  geom_bar(stat = 'identity') +
  facet_grid(vars(BOROUGH), scales = "free")



