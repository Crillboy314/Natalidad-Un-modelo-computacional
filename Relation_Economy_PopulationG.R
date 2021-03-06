####Generando cuadros de crecimiento poblaci�n

## REVISAR SIEMPRE LA DIRECCION DEL DIRECTORIO


library(readxl)
## Importo la base de Excel

Poblaciones_Andina <- read_excel("Poblaciones_Andina.xls" 
                                 ,sheet = "Hoja1")



## Creo los cuadros para las poblaciones de los cuatro pa�ses

par(mfrow = c(2,2))

plot(x=Poblaciones_Andina$A�o, y = Poblaciones_Andina$Colombia,
     type='l', ylab="Poblaci�n", xlab = "A�o",
     main= "Poblaci�n general de Colombia (1960-2017)")

plot(x=Poblaciones_Andina$A�o, y = Poblaciones_Andina$Ecuador,
     type='l', ylab="Poblaci�n", xlab = "A�o",
     main= "Poblaci�n general de Ecuador (1960-2017)")

plot(x=Poblaciones_Andina$A�o, y = Poblaciones_Andina$Peru,
     type='l', ylab="Poblaci�n", xlab = "A�o",
     main= "Poblaci�n general de Per� (1960-2017)")

plot(x=Poblaciones_Andina$A�o, y = Poblaciones_Andina$`Venezuela, RB`,
     type='l', ylab="Poblaci�n", xlab = "A�o",
     main= "Poblaci�n general de Venezuela, RB (1960-2017)")


## Creo los cuadros para las razones de crecimiento

Poblaciones_Andina_Porcentaje <- read_excel("Poblacion_Porcentaje.xls", 
                                 sheet = "Hoja1")

## Creo los cuadros para las poblaciones porcentuales de los cuatro pa�ses

par(mfrow = c(2,2))

plot(x=Poblaciones_Andina_Porcentaje$A�o, y = Poblaciones_Andina_Porcentaje$Colombia,
     type='l', ylab="% Crecimiento Poblaci�n", xlab = "A�o",
     main= "% crecimiento de la poblaci�n de Colombia (1960-2017)")

plot(x=Poblaciones_Andina_Porcentaje$A�o, y = Poblaciones_Andina_Porcentaje$Ecuador,
     type='l', ylab="% Crecimiento Poblaci�n", xlab = "A�o",
     main= "% crecimiento de la poblaci�n de Ecuador (1960-2017)")

plot(x=Poblaciones_Andina_Porcentaje$A�o, y = Poblaciones_Andina_Porcentaje$Peru,
     type='l', ylab="% Crecimiento Poblaci�n", xlab = "A�o",
     main= "% crecimiento de la poblaci�n de Per� (1960-2017)")

plot(x=Poblaciones_Andina_Porcentaje$A�o, y = Poblaciones_Andina_Porcentaje$`Venezuela, RB`,
     type='l', ylab="% Crecimiento Poblaci�n", xlab = "A�o",
     main= "% crecimiento de la poblaci�n de Venezuela, RB (1960-2017)")


## Empiezo a trabajar con series de tiempo

## Primero importo los datos de las econom�as

Economias <- read_excel("Economias.xls", 
                        sheet = "Hoja1")

## Luego creo las series de tiempo (add necesary libraries)

Ecuador <- ts(Poblaciones_Andina_Porcentaje$Ecuador, start = 1960, frequency = 1)
EconomiaEcuador <- ts(Economias$Ecuador, start = 1960, frequency = 1)


####### Grafico las series del cambio porcentual y de la econom�a
####### Luego analizo todos los datos para cada pa�s

library(stargazer)
library(tseries)

### Ecuador

## Esto primero solo para el an�lisis

par(mfrow = c(2,1))

plot(Ecuador, ylab = "% crecimiento de la poblaci�n",
     xlab = "A�o", main = "Serie de tiempo de la Tasa de Crecimiento")
plot(EconomiaEcuador, ylab = "PIB",
     xlab = "A�o", main = "Serie de tiempo del PIB (d�lares)")

##Esto si va
cor(Ecuador, EconomiaEcuador)

bfxecuador <- as.matrix(cbind(Ecuador,EconomiaEcuador))
po.test(bfxecuador)

bfxdiffecuador <- as.matrix(cbind(diff(Ecuador),diff(EconomiaEcuador)))
po.test(bfxdiffecuador)

