# Un equipo de biólogos quiere generar un modelo estadístico que permita identificar a que
#especie (a o b) pertenece un determinado insecto. Para ello se han medido tres variables
#(longitud de las patas, diámetro del abdomen y diámetro del órgano sexual) en 10 individuos
#de cada una de las dos especies.

# Obtención de los datos de entrenamiento

input <- ("
especie pata abdomen organo_sexual 
a 191 131 53
a 185 134 50
a 200 137 52
a 173 127 50
a 171 128 49
a 160 118 47
a 188 134 54
a 186 129 51
a 174 131 52
a 163 115 47
b 186 107 49
b 211 122 49
b 201 144 47
b 242 131 54
b 184 108 43
b 211 118 51
b 217 122 49
b 223 127 51
b 208 125 50
b 199 124 46
")

datos<- read.table(textConnection(input),header=TRUE)
datos$especie <-as.factor(datos$especie)

library(ggplot2)
library(ggpubr)

p1<- ggplot(data=datos, aes(x=pata,fill=especie)) +
  geom_histogram(position="identity",alpha=0.5)

p2<- ggplot(data=datos, aes(x=abdomen,fill=especie)) +
  geom_histogram(position="identity",alpha=0.5)

p3<- ggplot(data=datos, aes(x=organo_sexual,fill=especie)) +
  geom_histogram(position="identity",alpha=0.5)

ggarrange(p1,p2,p3,nrow = 3,common.legend = TRUE)


#A nivel individual, la longitud de la pata parece ser la variable que más se 
#diferencia entre especies (menor solapamiento entre poblaciones)

pairs(x=datos[,c("pata","abdomen","organo_sexual")],
      col=c("firebrick","green3")[datos$especie],pch=19
      )


# El par de variables abdomen-pata y el par pata-organo_sexual parecen separar bien las dos
#especies.


library(scatterplot3d)

scatterplot3d(x = datos$pata,
              y = datos$abdomen,
              z = datos$organo_sexual,
              color = c("firebrick", "green3")[datos$especie],
              pch = 19,
              grid = TRUE,
              xlab = "pata",
              ylab = "abdomen",
              zlab = "organo sexual",
              angle = 65,
              cex.axis = 0.6
              )

legend("topleft",
       bty="n",cex=0.8,title = "Especie",
       c("a","b"),fill=c("firebrick","green3"))

# La representación de las tres variables de forma simultánea parece indicar que las dos
#especies sí están bastante separadas en el espacio 3D generado.

# Prior probabilities

# Como no se dispone de información sobre la abundancia relativa de las especies a nivel
#poblacional, se considera como probabilidad previa de cada especie el número de
#observaciones de la especie entre el número de observaciones totales.

# Pia=Pib=10/20=0.5

# Homogeneidad de Varianza

# De entre los diferentes test que contrastan la homogeneidad de varianza, el más
#recomendable cuando solo hay un predictor, dado que se asume que se distribuye de forma
#normal, es el test de Barttlet. Cuando se emplean múltiples predictores, se tiene que
#contrastar que la matriz de covarianzas (∑) es constante en todos los grupos,
#siendo recomendable comprobar también la homogeneidad de varianza para 
#cada predictor a nivel individual.


# El test Box M fue desarrollado por el matemático Box (1949) como una extensión del 
#test de Barttlet para escenarios multivariante y permite contrastar la igualdad de 
#matrices entre grupos. El test Box M es muy sensible a violaciones de la normalidad 
#multivariante, por lo que esta debe ser contrastada con anterioridad. Ocurre con 
#frecuencia, que el resultado de un test Box M resulta significativo debido a la falta
#de distribución normal multivariante en lugar de por falta de homogeneidad en las 
#matrices de covarianza. Dada la sensibilidad de este test se recomienda emplear un 
#límite de significancia de 0.001.


# Distribución de los predictores de forma individual:

#Representacion mediante Histograma de cada variable para cada especie

par(mfcol=c(2,3))
for (k in 2:4){
  j0 <- names(datos)[k]
  x0<-seq(min(datos[,k]),max(datos[,k]),le=50)
  for (i in 1:2){
    i0<-levels(datos$especie)[i]
    x<- datos[datos$especie==i0,j0]
    hist(x,proba=T,col=grey(0.8),main=paste("especie",i0),xlab=j0)
    lines(x0,dnorm(x0,mean(x),sd(x)),col="red",lwd=2)
  }
}

# Representación de cuantiles normales de cada variable para cada especie 

for (k in 2:4){
  j0 <- names(datos)[k]
  x0 <- seq(min(datos[,k]),max(datos[,k]),le=50)
  for (i in 1:2){
    i0<- levels(datos$especie)[i]
    x<-datos[datos$especie==i0,j0]
    qqnorm(x,main=paste("especie",i0,j0),pch=19,col=i+1)
    qqline(x)
  }
}