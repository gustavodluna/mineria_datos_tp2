library(readr)
library(tidyverse)
library(RColorBrewer)
library(olsrr)


file <- "https://raw.githubusercontent.com/gustavodluna/mineria_datos_tp2/main/detroit.csv"

detroit <- readr::read_csv(file)

detroit

dim(detroit)

head(detroit)

summary(detroit)

str(detroit)

##################################################################################
### Consigna 1: Cuál es el nivel de asociación lineal de las variables predictoras 
### con la variable H? Comentar.
##################################################################################

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




##################################################################################
### Consigna 2:Realizar una regresión lineal múltiple, seleccionando los mejores 
#predictores entre las variables independientes disponibles, utilizando un método
#de selección automática.Describir el proceso de selección automática utilizado.
#(Sug. Considerar como probabilidad de entrada 0.10 y de salida del modelo 0.15.)
##################################################################################

# Siguiendo con las consignas, y en busca de determinar la ecuacion de regresion
#multiple seleccionando los mejores predictores, generamos un modelo que incluya
#todas las variables en estudio.


model_detroit <- lm(H~ FTP + UNEMP + M + LIC + GR + CLEAR + 
                      W + NMAN + G + HE + WE, data=detroit)

summary(model_detroit)

# De este modelo vemos que los valores P de las variables predictoras son mayores a nuestro
#α=0.05, por lo que no son estadísticamente significativas. Sin embargo, 
#nuestro R2=0.9997 nos indica que la variación de H es explicada o determinada por 
#nuestro modelo de regresión, aunque al agregar más variables al modelo de regresión 
#el R2 aumentará siempre, incluso si esas nuevas variables son sólo ruido. 
#Para subsanar este inconveniente tenemos un Coficiente de Determinación Ajustado R2=0.9969
#que nos indicaría la proporción de la variación de H estaría explicada por el modelo, ajustando
#para la cantidad de variables que se utilizan. Por otro lado,
#se nos presenta el resultado del Test de ANOVA para la Regresión Lineal con un
#estadístico de prueba F y un p.value=0.04127


# Consideremos las hipótesis de prueba:

#     H0: la regresion no es significativa
#     H1: la regresion es significativa

# Con un valor P<α=0.05
#rechazamos la hipótesis nula, es decir que la regresión resulta significativa y continuamos con
#el proceso.
#Esto nos lleva a pensar que hay variables intervinientes que no permiten avanzar u ocultan
#otras relaciones posibles entre las variables, por ello debemos volver a un punto anterior y
#descartar como variables predictoras a UNEMP, CLEAR y W . Generamos un nuevo modelo:

model_detroit2 <- lm(H~ FTP + M + LIC + GR  + 
                      NMAN + G + HE + WE, data=detroit)

summary(model_detroit2)


# Vemos que el R^2 ajustado arroja un valor de 0.9878, si bien es un poco menor al modelo
#anterior sigue siendo significativo en su nivel de determinación. Por otro lado, obtemos un
#valor P=0.0001641
#que resulta considerablemente menor que antes. Tampoco vemos que los valores P de las
#variables predictoras nos indiquen que éstas son estadísticamente significativas, pero
#decidimos continuar igual con la selección automática de variables para luego estudiar los
#residuos, la multicolinealidad y finalmente tomar alguna decisión respecto al modelo.

# Utilizaremos el método de selección automática Stepwise sobre un modelo que incluya todas
#las variables, utilizando las probabilidades de entrada y salida del modelo sugeridas 
#en la consigna (Entrada: 0.10 y Salida: 0.15):

(stepwise <- ols_step_both_p(model_detroit,pent=0.10,prem=0.15))
#install.packages("olsrr")

packageVersion("olsrr")
#install.packages("remotes")
#remotes::install_version("olsrr", version = "0.6.0") 
#install.packages("D:/Descargas/olsrr-0.5.3.tar.gz", repos = NULL, type = "source")

# El modelo final incluye dos variables predictoras, LIC y FTP, que explican el 99.0% 
#de la variabilidad de la variable dependiente, lo cual es un muy buen ajuste. 
#Ambos predictores son estadísticamente significativos y mejoran considerablemente 
#el ajuste del modelo en comparación con el modelo base.

(step_sum<-data_frame(
  Step=1:2,
  Order=stepwise$orders,
  Method=stepwise$method,
  Adj_R_2=stepwise$adjr))


library(tibble)

step_sum <- tibble(
  Step = 1:2,
  Order = stepwise$orders,
  Method = stepwise$method,
  Adj_R_2 = stepwise$adjr
)
step_sum


plot(x=step_sum$Step,y=step_sum$Adj_R_2,type="o",
     xlab="Modelo de Stepwise",
     ylab="R2 ajustado")

# En el siguiente paso analizaremos la multiconealidad del modelo, esto coincide
#con la consigna 7 del ejercicio: Analizar la colinealidad de las variables
#predictoras presentes en la ecuacion.

model_detroit <-lm(H~ LIC + FTP, data=detroit)
ols_vif_tol(model_detroit)

# Este fenomeno ocurre cuando las variables predictoras estan relacionadas entre si, lo que
#implicaria diversos efectos sobre nuestro modelo: Limita el tamaño de R2 porque
#los predictores correlacionados "explican" la misma variacion; Dificulta la
#determinacion de la importancia de los predictores; Incrementa la varianza de los
#estimadores de lso parametros.

# Vemos que para las variables predictoras no se obtienen valores de VIF > 10 y de 
#Tolerancia <0.1 que nos indicaría que estamos en presencia de un fenómeno 
#de multicolinealidad.

