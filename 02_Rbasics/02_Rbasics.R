# vamos a trabajar con datos de elecciones de US en R
#install.packages("dlplyr")
#install.packages("lubridate")
#install.packages("ggplot2")
#install.packages("zoo")

library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2)

#ponemos path de trabajo
setwd("~/Documentos/CursoR/02_Rbasics/")

# cargamos los datos
approval_polls <-read.csv("data/polls.csv",sep = ";")

# hacemos un pequeño insight de los datos
head(approval_polls)
# numero de filas
nrow(approval_polls)
# numero de columnas
length(approval_polls)
# info de la structura
str(approval_polls)

# seleccionamos columnas
approval_polls %>% 
  select("President","Date","Approve") %>%
  head()

# de otra manera
approval_polls %>% 
  select(President,Date,Approve) %>%
  head()



# filtramos solo para Trump
approval_polls %>% 
  select(President,Date,Approve) %>%
  filter(President == "Trump")
# ejercicio, filtra para  hillary

# evaluemos por presidentes el promedio de su nota
approval_polls %>%
  group_by(President) %>%
  summarise(Approve = mean(Approve))

# Podemos extraer la Approve columna como vector
# y guardarlo como una columna con pull
TrumpApproval <- approval_polls %>% 
  select(President,Date,Approve) %>%
  filter(President == "Trump") %>%
  pull(Approve)
# si hacemos promedio vemos lo de antes
mean(TrumpApproval)

# Ejercicio
# Realiza el promedio para cada presidente de la columna Disapprove


#usamos lubridate package para formatear fechas
# Seleccionamos columnas President, Date, Approve de approval_polls y filtramos por la presidencia de Trump
TrumpPolls <- approval_polls %>% 
  select(President,Date,Approve) %>%
  filter(President == "Trump")

# usamos months() y mdy() para extraer el mes donde se ha realizado la encuesta
# Agrupamos por mes y hacemos un resumen de Approve
TrumpPolls %>%
  mutate(Month = months(mdy(Date))) %>%
  group_by(Month) %>%
  summarise(Approve = mean(Approve))

# guardamos el approval de Trump modificando la fecha y ordenando por la misma
TrumpApproval <- approval_polls %>% 
  filter(President == "Trump") %>%
  mutate(Date = mdy(Date)) %>%
  arrange(Date) 

# usamos la funcion rollmean() del paquete zoo para obtener una rolling mean de las 10 ultimas encuestas
TrumpApproval <- TrumpApproval %>%
  mutate(AvgApprove = rollmean(Approve, 10, na.pad=TRUE, align = "right"))

# usamos ggplot para plotear los datos
ggplot(data = TrumpApproval, aes(x=Date,y=AvgApprove)) + 
  geom_line()+ggtitle("Trump Approval") + xlab("Dias") +
  ylab("Avg Aprrove") 


# Hacemos lo mismo ahora para todos los presidentes
AllApproval <- approval_polls %>%
  group_by(President) %>%
  mutate(AvgApprove = rollmean(Approve, 10, na.pad=TRUE, align = "right"))


# ploteamos
ggplot(data = AllApproval, aes(x=Days, y=AvgApprove, col=President)) + 
  geom_line()+ggtitle("All Approval")


