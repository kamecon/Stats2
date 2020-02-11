### Distribución de la varianza muestral #### 

# A diferencia de antes creamos una lista en lugar de un vector, en cada elemento de la lista guardamos las 1000 realizaciones de la varianza de la muestra

varianza_vec <- list()
tamaño <- c(40,100,1000,10000,100000,500000)
for (j in tamaño) {
  for (i in 1:1000) {
    varianza_vec[[as.character(j)]][i] <- var(sample(x = pop, size = tamaño, replace = TRUE))
  }
}

# Usaremos algunas funciones y librerías útiles de la librería `tidyverse`

library(tidyverse)

# Transformamos la lista en una tabla (arreglo rectangular)

varianza <- as.tibble(varianza_vec)

# Convertimos la tabla en formato long o "tidy"

varianza2 <- varianza %>%
  gather(key = "Tamaño") 

varianza2$Tamaño <- factor(x = varianza2$Tamaño, levels = c(40,100,1000,10000,100000,500000))

# Hacemos el gráfico usando la librería ggplot

varianza_grafico <- ggplot(data = varianza2) +
  geom_histogram(aes(x=value, y=..density..), fill="white", colour="black") +
  geom_density(aes(x=value)) +
  facet_wrap(~Tamaño) +
  geom_vline(xintercept = 225, linetype="dotted", color = "blue", size=1.5) +
  labs(title = 'Distribución muestral de la varianza',  x ='Varianza', y = 'Frecuencia') +
  theme(plot.title=element_text(face="bold", hjust=0.5, vjust=2, colour="#3C3C3C"))

varianza_grafico

# Verificamos la normalidad de la distribución muestral de la varianza

varianza %>% 
  ggplot(aes(sample=`5e+05`))+ stat_qq()+ geom_qq_line()

ggplot(varianza2, aes(sample = value)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Tamaño)

# Distribución chi cuadrado

# La distribución chi cuadrado es una función "deducida de la normal", es decir, que se obtiene a partir de manipulaciones de realizaciones de la distribución normal. Especificamente, la chi cuadrado es el resultado de la suma de variables aleatorias normales estándar elevadas al cuadrado, replicamos este procedimiento con R

# Creamos un vector vacío
chicuadrado <- vector()

# Con un bucle rellenamos el vector anterior con sumas de 10 variables aleatorias estándar al cuadrado (x_1)^2+(x_2)^2+...+(x_10)^2, repetimos esta operación mil veces y vemos que pinta tiene la distribución de esta variable

for (i in 1:1000) {
  chicuadrado[i] <- sum(rnorm(10)^2)
}

# Miramos el histograma

hist(chicuadrado)

# Hacemos otro histograma con ggplot

ggplot(data = data.frame(chicuadrado), aes(x=chicuadrado, y=..density..)) +
  geom_histogram(fill="cornsilk", colour="grey60", size=.2) +
  geom_density()
