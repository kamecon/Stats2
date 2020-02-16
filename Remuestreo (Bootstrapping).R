### Remuestreo (Bootsrapping) ###

# Hasta ahora hemos estado creando distribuciones muestrales de un estadístico extrayendo repetidamente un número determinado de muestras del mismo tamnaño, era este proceso de muestreo repetido el que generaba variación en el estadístico y lo convertía en una variable aleatoria (p.e una media era distinta en cada muestra y analizabamos su distribución).
#No obstante, en la vida real solo disponemos de una muestra, por lo que no disponemos de una distribución muestral del estadístico, ni intervalos de confianza.
# ¿Es posible replicar la variabilidad generada por un muestreo repetido empleando solo una muestra? La respuesta es SI, usando la técnica de remuestreo. Tal y como su nombre lo inidica la idea es obtener muestras repetidas a partir no de la población sino de una muestra.

# Creamos una muestra con media 10 y desviación estándar 5

muestra1 <- rnorm(100, mean = 1, sd = 0.5)

# Esta es la única muestra que tenemos, por lo que no tenemos una distribución muestral de un estadístico (la media por ejemplo) ni podemos calcular el grado de incertidumbre de dicho esadístico con un intervalo de confianza. Para poder hacer lo anterior vamos a generar *muestras a partir de la muestra*
# Haremos lo anterior tomando muestras *con reemplazo* del mismo tamaño de la muestra, es decir, repetir un número determinado de veces la siguiente operación

mean(sample(x = muestra1, size = length(muestra1), replace = TRUE))

# Por ejemplo, podemos repetir la operación anterior 1000 veces

remuestreo <- replicate(n = 1000, expr = mean(sample(x = muestra1, size = length(muestra1), replace = TRUE)))

hist(remuestreo, main = "Distribución de las medias muestrales bajo un remuestreo de 1000 repeticiones", xlab = "", ylab = "Frecuencia")
abline(v = mean(muestra1), lty = 2, lwd = 3, col = "red")

# A partir de este proceso de muestreo basado en una sola muestra podemos obtener intervalos de confianza

library(lsr)

ciMean(remuestreo, conf = 0.95)
