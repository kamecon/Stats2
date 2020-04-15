### Intervalos de confianza ###

# A continuación creamos una población de tamaño 1.000.000 que se distribuye como una normal con media 100 y desviación estandar 15

pop <- rnorm(1000000, mean = 100, sd = 15)

# Para calcular un intervalo de confianza del 95% de la población usamos la función `qnorm` en el caso de una función normal

qnorm(p = c(0.025, .975), mean = 100, sd = 15)

# Para calcular los intervalos de confianza de la *media muestral* vamos a utilizar la función `ciMean` de la librería `lsr` correspondiente a uno de los libros de la bibliografía "Learning statistics with R" https://learningstatisticswithr.com/lsr-0.6.pdf

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


# Intervalos de confianza (ejercicios)

library(ggplot2)

# Definimos los datos del problema (Ejemplo 8.4 del libro, página 307)
media <- 198
des_tip <- 12
n <- 25

confianza <- 0.99
alpha_2 <- (1-confianza)/2
z_alhpa_2 <- qnorm(alpha_2, lower.tail = FALSE)
ME <- z_alhpa_2*(des_tip / sqrt(n))
Limite_inferior <- media - ME
Limite_superior <- media + ME

#Mostramos los datos en una tabla

data.frame(
  "Concepto" = c("Nivel de confianza", "Valor Z", "Limite inferior", "Limite superior"),
  "Datos"=c(confianza, z_alhpa_2, Limite_inferior, Limite_superior)
  )

# Hacemos el gráfico usando ggplot2

# Creamos dos vectores, uno con los datos de la variable (empleo un vector que va desde la media menos 5 veces la desviación típica muestral hasta la media mas 5 veces la desviación típica muestral)

lim_inf_graf <- media - 5*(des_tip / sqrt(n))
lim_sup_graf <- media + 5*(des_tip / sqrt(n))

x <- seq(from = lim_inf_graf, to = lim_sup_graf, by = 0.05 )

# Creamos la distribución normal estándar basados en los datos creados en el paso anterior
y <- dnorm((x - media) / (des_tip / sqrt(n)))

# Juntamos ambos vectores y hacemos una tabla (data frame). La libreria ggplot solo toma como argumentos arreglos tabulates de este tipo
df_dist <- data.frame(x=x, y=y)

# Gráfico
ggplot(df_dist, aes(x = x, y = y)) +
  geom_line()  +
  geom_vline(xintercept = Limite_inferior) +
  geom_vline(xintercept = Limite_superior) +
  geom_area(mapping = aes(ifelse(x>Limite_inferior & x<Limite_superior,x,0)), fill = "turquoise2", alpha = 0.8) +
  xlim(lim_inf_graf,lim_sup_graf) +
  annotate("text", x = media, y = 0.15, label = paste0(as.character(confianza*100), "% de confianza"))


# Intevalos de confianza con varianza desconocida

# Distribución t

# Creamos una distribución chi cuadrado a partir de su definición, primero creamos una chi cuadrado al igual que en la sección anterior, simulamos la suma de una sumatoria de variables normales estandar al cuadrado

chicuadrado <- vector()

for (i in 1:1000) {
  chicuadrado[i] <- sum(rnorm(10)^2)
}

# Esta sería la distribución chi cuadrado

hist(chicuadrado, xlab = "", ylab = "Frecuencia", main = "Distribución Chi Cuadrado simulada", breaks = 30)

# Teniendo una chi cuadrado, creamos una distribución t a partir de su definición, primero creamos una chi cuadrado "corregida" que no es más que la chi cuadrado que hemos creado dividida por sus grados de libertad.

chi_corregida <- chicuadrado / (length(chicuadrado) - 1)

# La distribución t se obtiene de la división de una normal estándar entre la raíz cuadrada de una chi cuadrado entre sus grados de libertad, la chi cuadrada que hemos construido arriba

t <- rnorm(1000) / sqrt(chi_corregida) 

hist(t, xlab = "", ylab = "Frecuencia", main = "Distribución t simulada", breaks = 30)


# Creamos una distribución normal para comparar con las distribuciones t. Fuente: https://www.statmethods.net/advgraphs/probability.html

x <- seq(-4, 4, length=100)
normal <- dnorm(x)

# Creamos varias distribuciones t con distintos grados de libertad y representamos junto a una normal

grados_libertad <- c(1, 3, 8, 30)
colores <- c("red", "blue", "darkgreen", "gold", "black")
labels <- c("df=1", "df=3", "df=8", "df=30", "normal")

plot(x, normal, type="l", lty=2, xlab=" ",
     ylab=" ", main="Comparaciones Distribuciones t")

for (i in 1:4){
  lines(x, dt(x,grados_libertad[i]), lwd=2, col=colores[i])
}

legend("topright", inset=.05, title="Distribuciones", labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colores)
