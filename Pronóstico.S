########Pron�stico de demanda en Rstudio########

#Seleccionar carpeta de trabajo
setwd("C:/R")

#Paquetes
install.packages(c("readxl","tidyverse","tseries","urca","forecast"))

library(readxl)

library(tidyverse)

library(tseries)

library(urca)

library(forecast)

#Importar datos
Demanda <- read_excel("data.xlsx")

print(Demanda)

Demanda <- ts(Demanda$`Sales (Units)`, start = c(2014,01), frequency = 12)

print(Demanda)

# Gr�fico de los datos
ts.plot(Demanda, main="Demanda Vs Tiempo",xlab="Tiempo")

# Prueba de correlaci�n
acf(Demanda, main="Serie no estacionaria")

ur.kpss(Demanda) %>% summary()

ndiffs(Demanda)

ur.kpss(diff(Demanda)) %>% summary()

# grafica de los datos diferenciados
ts.plot(diff(Demanda), main="Serie estacionaria", xlab="Tiempo", ylab="Demanda(diff)")

plot(Demanda, main="Original", xlab="Tiempo")

# Modelo ARIMA


# AR Modelo Autorregresivo
# MA Modelo Medias M�viles

# Selecci�n modelo ARIMA
modelo <- auto.arima(Demanda, trace = F)
summary(modelo)
checkresiduals(modelo) 


# Pron�stico
autoplot(forecast(modelo, h=9),main="Pron�stico", xlab="Tiempo")

forecast(modelo, h=9)

###FIN###