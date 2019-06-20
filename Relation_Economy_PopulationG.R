####Generando cuadros de crecimiento población

## REVISAR SIEMPRE LA DIRECCION DEL DIRECTORIO


library(readxl)
## Importo la base de Excel

Poblaciones_Andina <- read_excel("Poblaciones_Andina.xls" 
                                 ,sheet = "Hoja1")



## Creo los cuadros para las poblaciones de los cuatro países

par(mfrow = c(2,2))

plot(x=Poblaciones_Andina$Año, y = Poblaciones_Andina$Colombia,
     type='l', ylab="Población", xlab = "Año",
     main= "Población general de Colombia (1960-2017)")

plot(x=Poblaciones_Andina$Año, y = Poblaciones_Andina$Ecuador,
     type='l', ylab="Población", xlab = "Año",
     main= "Población general de Ecuador (1960-2017)")

plot(x=Poblaciones_Andina$Año, y = Poblaciones_Andina$Peru,
     type='l', ylab="Población", xlab = "Año",
     main= "Población general de Perú (1960-2017)")

plot(x=Poblaciones_Andina$Año, y = Poblaciones_Andina$`Venezuela, RB`,
     type='l', ylab="Población", xlab = "Año",
     main= "Población general de Venezuela, RB (1960-2017)")


## Creo los cuadros para las razones de crecimiento

Poblaciones_Andina_Porcentaje <- read_excel("Poblacion_Porcentaje.xls", 
                                 sheet = "Hoja1")

## Creo los cuadros para las poblaciones porcentuales de los cuatro países

par(mfrow = c(2,2))

plot(x=Poblaciones_Andina_Porcentaje$Año, y = Poblaciones_Andina_Porcentaje$Colombia,
     type='l', ylab="% Crecimiento Población", xlab = "Año",
     main= "% crecimiento de la población de Colombia (1960-2017)")

plot(x=Poblaciones_Andina_Porcentaje$Año, y = Poblaciones_Andina_Porcentaje$Ecuador,
     type='l', ylab="% Crecimiento Población", xlab = "Año",
     main= "% crecimiento de la población de Ecuador (1960-2017)")

plot(x=Poblaciones_Andina_Porcentaje$Año, y = Poblaciones_Andina_Porcentaje$Peru,
     type='l', ylab="% Crecimiento Población", xlab = "Año",
     main= "% crecimiento de la población de Perú (1960-2017)")

plot(x=Poblaciones_Andina_Porcentaje$Año, y = Poblaciones_Andina_Porcentaje$`Venezuela, RB`,
     type='l', ylab="% Crecimiento Población", xlab = "Año",
     main= "% crecimiento de la población de Venezuela, RB (1960-2017)")


## Empiezo a trabajar con series de tiempo

## Primero importo los datos de las economías

Economias <- read_excel("Economias.xls", 
                        sheet = "Hoja1")

## Luego creo las series de tiempo (add necesary libraries)

Ecuador <- ts(Poblaciones_Andina_Porcentaje$Ecuador, start = 1960, frequency = 1)
EconomiaEcuador <- ts(Economias$Ecuador, start = 1960, frequency = 1)


####### Grafico las series del cambio porcentual y de la economía
####### Luego analizo todos los datos para cada país

library(stargazer)
library(tseries)

### Ecuador

## Esto primero solo para el análisis

par(mfrow = c(2,1))

plot(Ecuador, ylab = "% crecimiento de la población",
     xlab = "Año", main = "Serie de tiempo de la Tasa de Crecimiento")
plot(EconomiaEcuador, ylab = "PIB",
     xlab = "Año", main = "Serie de tiempo del PIB (dólares)")

##Esto si va
cor(Ecuador, EconomiaEcuador)

bfxecuador <- as.matrix(cbind(Ecuador,EconomiaEcuador))
po.test(bfxecuador)

bfxdiffecuador <- as.matrix(cbind(diff(Ecuador),diff(EconomiaEcuador)))
po.test(bfxdiffecuador)

