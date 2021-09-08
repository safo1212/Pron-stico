


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

Demanda <- ts(Demanda, start = c(2014,01), frequency = 12)

print(Demanda)

# Grafico de datos en niveles
ts.plot(Demanda)

# Prueba de correlación
ur.kpss(Demanda) %>% summary() #los datos en niveles no son estacionarios
ur.kpss(diff(Demanda)) %>% summary() # los datos diferenciados si son estacionarios

# grafica de los datos diferenciados
ts.plot(diff(Demanda))

##
# Modelo ARIMA
##

# AR representa una regresion de la variable contra valores pasados de ella misma
# MA representa la media movil ponderada de los errores pasados del pronóstico.

# Combinando los dos modelos
# p = parte autorregresiva
# d = grado de difenrenciacion
# q = parte media movil

# Selección automatica del modelo ARIMA
modelo <- auto.arima(Demanda, seasonal=T, stepwise=T)
summary(modelo)
checkresiduals(modelo) # Los residuales no estan correlacionados


# Pronóstico
autoplot(forecast(modelo, h=9))

###FIN###