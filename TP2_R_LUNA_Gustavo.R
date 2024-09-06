library(readr)
library(tidyverse)
library(RColorBrewer)

file <- "https://raw.githubusercontent.com/gustavodluna/mineria_datos_tp2/main/detroit.csv"

detroit <- readr::read_csv(file)

detroit

dim(detroit)

head(detroit)

summary(detroit)

str(detroit)


### Consigna 1: Cuál es el nivel de asociación lineal de las variables predictoras 
### con la variable H? Comentar.

M <- cor(detroit[2:13])
M


corrplot::corrplot(M, type="upper",
                   order="original",
                   addCoef.col = "black",
                   method = "color",
                   col=brewer.pal(n=8,name="RdYlBu"))

#  Se observa que para la variable de respuesta H 
#(Tasa de Homicidios cada 100000 habitantes) la mayoria de las variables tienen
#una correlacion cercana a 1 o -1, entendemos que estariamos en condiciones
#de descartar para el proximo analisis de regresion multiples a las variables
#con un R mas bajo que no serian optimas candidatas como variables predictoras,
#estas son: 
#- UNEMP (Porcentaje de desempleados) con un R=0.34
#- CLEAR (Porcentaje de homicidios resueltos con arresto del responsable) con un R=0.15
#- W (Cantidad de hombres blancos en la poblacion) con un R=0.12


# De forma similar, analizaremos el siguiente grafico de dispersion para todas las
#variables en analisis:

pairs(detroit[2:13],pch=19,lower.panel = NULL)

# Para un analisis mas preciso, utilizaremos los diagramas de dispersion de H con
#las variables predictoras

pivot_detroit <- detroit[2:13] %>%
  pivot_longer(-H,names_to = "var",values_to = "value")

ggplot(pivot_detroit, aes(x=value,y=H)) +
  geom_point() +
  stat_smooth(method = "lm") +
  facet_wrap(~ var,scales = "free")

# Realizando un examen visual de los diagramas de dispersion, pareciera que las variables
#UNEMP, CLEAR y W no seguirian un patron lineal de relacion respecto de H. Debido a esto
#y al resultado del coeficiente R, podriamos pensar en excluir las variables
#UNEMP, CLEAR y W de nuestro modelo.