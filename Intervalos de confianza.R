### Intervalos de confianza ###

# A continuación creamos una población de tamaño 1.000.000 que se distribuye como una normal con media 100 y desviación estandar 15

pop <- rnorm(1000000, mean = 100, sd = 15)

# Para calcular un intervalo de confianza del 95% de la población usamos la función `qnorm` en el caso de una función normal

qnorm(p = c(0.25, .975), mean = 100, sd = 15)

# Para calcular los intervalos de confianza de la *media* vamos a utilizar la función `ciMean` de la librería `lsr` correspondiente a uno de los libros de la bibliografía "Learning statistics with R" https://learningstatisticswithr.com/lsr-0.6.pdf

#Cargamos la librería

library(lsr)

# Calculemos el intervalo de confianza al 95% de la población

ciMean(pop, conf = 0.95)

# Hagamos lo mismo con una muestra de la población de tamaño 100

sample1 <- sample(x = pop, size = 100)

ciMean(sample1, conf = 0.95)

# Vamos a generar 1000 muestras de la población de tamaño 100 y calculamos los intervalos de confianza correspondientes a cada una de las muestras 

# Definimos primero tres vectores vacíos cuyo tamaño es es igual al número de muestras que deseamos tomar, cada uno corresponde a la media y los límites inferior y superior del intervalo de confianza

medias_100 <- vector(length = 1000)
cilow <- vector(length = 1000)
cihigh <- vector(length = 1000)

# Hacemos el bucle que nos permite repetir 1000 veces el proceso de tomar una muestra

for (i in seq_along(medias_100)) {
  x <- sample(x = pop, size = 100, replace = TRUE)
  medias_100[i] <- mean(x)
  ci <- ciMean(x)
  cilow[i] <- ci[1]
  cihigh[i] <- ci[2]
}

# Lo representamos gráficamente (gráfico tomado de la página 323 del libro "Learning statistics with R", código original en https://github.com/djnavarro/rbook/blob/master/original/scripts/confidenceIntervals.R )

emphColLight <- rgb(.5,.5,1)
emphCol <- rgb(0,0,1)

plot.new()
plot.window(xlim=c(0,101),ylim=c(95,105))
for( i in seq_along(medias_100) ){
  col <- "grey30"
  if( cilow[i]>100 | cihigh[i]<100 ) {
    col <- emphCol
    lines(i,75,type="p",pch=8,lwd=2)
  }
  lines(c(i,i), c(cilow[i],cihigh[i]), col=col, lwd=2)
  lines(i,medias_100[i],pch=19,type="p",col=col)
}
axis(1)
axis(2)
title(xlab="Número de simulación",ylab="Media",main=paste0("Tamaño de la muestra = ",100),font.main=1)
abline(h=100,lty=2)

# Gráfico dinámico

# Usamos la librería `animation` que posee algunas animaciones de temas estadísticos (también de optimización, métodos numéricos, machine learning, etc.)

library("animation")

# Definimos las opciones, primero el intervalo de tiempo entre cada iteración y luego el número de iteraciones, que en nuestro caso es el número de simulaciones
ani.options(interval = 0.1, nmax = 100)

# Creamos simulaciones de intervalos de confianza del 95%
conf.int(0.9, main = "Intervalos de confianza del 95%")
