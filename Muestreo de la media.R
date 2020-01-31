### Distribución de las medias muestrales en el muestreo ###

# A continuación creamos una población de tamaño 1.000.000 que se distribuye como una normal con media 100 y desviación estandar 15

pop <- rnorm(1000000, mean = 100, sd = 15)

# Representamos la población mediante un histograma

hist(pop, freq = FALSE, breaks = 20, main = "Histograma y densidad de la Población", xlab = "", ylab = "Frecuencia Relativa"); curve(dnorm(x, mean = mean(pop), sd = sd(pop)), add=TRUE, col="darkblue")

# Tomamos una muestra de tamaño 100 de la población, es decir, seleccionamos de manera aleatoria 100 elementos de la población

sample1 <- sample(x = pop, size = 100)

#EJERCICIO: Calcule en la consola varias veces la media repitiendo la instrucción anterio y usando la función `mean()`

# Tomamos más muestras de mayor tamaño de la población 

sample2 <- sample(x = pop, size = 10000)
sample3 <- sample(x = pop, size = 100000)
sample4 <- sample(x = pop, size = 250000)

# Vamos a repetir 1000 veces (¡!) el proceso de tomar una muestra de tamaño 100, y para cada muestra calculamos la media. Haremos esto empleando un bucle (loop)

# Definimos primero un vector vacio cuyo tamaño es es igual al número de muestras que deseamos tomar

#NOTA DE R: Un vector no es más que un contenedor de datos homogéneos, como lo que tenemos en la cabeza que es un vector [1,2,3], es el equivalente a un array en Python

medias_100 <- vector(length = 1000)

# Hacemos el bucle que nos permite repetir 1000 veces el proceso de tomar una muestra

#NOTA DE R: El uso de bucles en R no es muy popular o aconsejado, pero para casos no complejos no representa un problema

for (i in seq_along(medias_100)) {
  medias_100[i] <- mean(sample(x = pop, size = 100))
}

#NOTA DE R: En lugar de la función `seq_along`se puede usar una notación más convencional en otros lenguajes como for (i in 1:1000), que significa "repite esta operación en índices que van del 1 al 1000 en saltos de 1" que es una manera sofisticada de decir "repite esta operación 1000 veces"  

# Representamos el histograma

hist(medias_100, main = "Tamaño de muestra N=100", xlab = "", ylab = "Frecuencia", xlim = c(95,105))

#EJERCICIO: ¿Como interpreta el histograma? ¿Representa una distribución de qué?

#EJERCICIO: ¿Como interpreta la media?

#EJERCICIO: ¿Qué diferencias en término de amplitud observa con la distribución de la población?

#Repetimos el ejercicio anterior con muestras de mayor tamaño

medias_10000 <- vector(length = 1000)
for (i in seq_along(medias_10000)) {
  medias_10000[i] <- mean(sample(x = pop, size = 10000))
}

medias_100000 <- vector(length = 1000)
for (i in seq_along(medias_100000)) {
  medias_100000[i] <- mean(sample(x = pop, size = 100000))
}

medias_250000 <- vector(length = 1000)
for (i in seq_along(medias_250000)) {
  medias_250000[i] <- mean(sample(x = pop, size = 250000))
}

# Representamos graficamente los 4 ejercicios

#graphics.off()

par(mfrow=c(2,2))
hist(medias_100, main = "Tamaño de muestra N=100", xlab = "", ylab = "Frecuencia", xlim = c(95,105))
hist(medias_10000, main = "Tamaño de muestra N=10000", xlab = "", ylab = "Frecuencia", xlim = c(99,101))
hist(medias_100000, main = "Tamaño de muestra N=100000", xlab = "", ylab = "Frecuencia", xlim = c(99.5,100.5))
hist(medias_250000, main = "Tamaño de muestra N=250000", xlab = "", ylab = "Frecuencia", xlim = c(99.7,100.3))

#EJERCICIO: ¿Qué puede comentar acerca de como evoluciona la amplitud de las distribuciones a medida que aumenta el tamaño de la muestra?

#EJERCICIO: ¿Cual es la interpretación estadística de lo anterior?