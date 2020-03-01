### Distribución de la varianza muestral #### 

# A diferencia de antes creamos una lista en lugar de un vector, en cada elemento de la lista guardamos las 1000 realizaciones de la varianza de la muestra

pop <- rnorm(1000000, mean = 100, sd = 15)

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

#varianza %>% 
 # ggplot(aes(sample=`5e+05`))+ stat_qq()+ geom_qq_line()

ggplot(varianza2, aes(sample = value)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Tamaño)

# Distribución chi cuadrado

# La distribución chi cuadrado es una función "deducida de la normal", es decir, que se obtiene a partir de manipulaciones de realizaciones de la distribución normal. Especificamente, la chi cuadrado es el resultado de la suma de variables aleatorias normales estándar elevadas al cuadrado, replicamos este procedimiento con R

#Generamos una realización de una variable normal estándar
rnorm(1, mean = 0, sd = 1)

#Nos debería dar en media un número relativamente cercano a su media que es cero. Si elevamos dicho número al cuadrado deberíamos obtener también un número pequeño en el caso (más probable) que obtengamos un número entre cero y uno (68% de la distribución)
rnorm(1, mean = 0, sd = 1)^2

#Ahora en lugar de una sola realización veamos que ocurre con la suma de 2
a1 <- rnorm(2, mean = 0, sd = 1)
a1
#Sumamos los cuadrados
sum(a1^2)

#Empezamos a obtener números más grandes, ya que sumamos más de una realización

#A medida que aumentamos el número de realizaciones (grados de libertad) mayor será la probabilidad de obtener un número mayor
#df=5
a5 <- rnorm(5, mean = 0, sd = 1)
a5
sum(a5^2)

#df=10
a10 <- rnorm(10, mean = 0, sd = 1)
a10
sum(a10^2)

#Veamos como esto determina la forma de los histogramas de este proceso. En el caso de 10 realizaciones o grados de libertad
# Creamos un vector vacío
chicuadrado <- vector()

# Con un bucle rellenamos el vector anterior con sumas de 10 variables aleatorias estándar al cuadrado (x_1)^2+(x_2)^2+...+(x_10)^2, repetimos esta operación mil veces y vemos que pinta tiene la distribución de esta variable

for (i in 1:1000) {
  chicuadrado[i] <- sum(rnorm(10)^2)
}

# Miramos el histograma

hist(chicuadrado, xlab = "", ylab = "", main = "Chi-cuadrado con 10 grados de libertad", freq = FALSE)

# Hacemos otro histograma con ggplot

ggplot(data = data.frame(chicuadrado), aes(x=chicuadrado, y=..density..)) +
  geom_histogram(fill="cornsilk", colour="grey60", size=.2) +
  geom_density()

#Veamos como cambia la forma de la distribución con los cambios de los grados de libertad

chi_vec <- list()
gl <- c(2,5,10,20)
for (j in gl) {
  for (i in 1:1000) {
    chi_vec[[as.character(j)]][i] <- sum(rnorm(j)^2)
  }
}

chi_tib <- as_tibble(chi_vec)

chi_tib2 <- chi_tib %>%
  gather(key = "df") 

niveles <- chi_tib2$df %>% 
  unique() %>%
  as.numeric()

chi_tib2$df <- factor(x = chi_tib2$df, levels = niveles)

chi_grafico <- ggplot(data = chi_tib2) +
  geom_histogram(aes(x=value, y=..density..), fill="white", colour="black") +
  geom_density(aes(x=value)) +
  facet_wrap(~df) +
  labs(title = 'Distribuciones chi cuadrado con distintos grados de libertad',  x ='', y = 'Frecuencia') +
  theme(plot.title=element_text(face="bold", hjust=0.5, vjust=2, colour="#3C3C3C"))

chi_grafico

chi_grafico2 <- ggplot(data = chi_tib2) +
  geom_density(aes(x=value, group=df, colour=df)) +
  labs(title = 'Distribuciones chi cuadrado (simulación) con distintos grados de libertad',  x ='', y = 'Frecuencia') +
  theme(plot.title=element_text(hjust=0.5, vjust=2, colour="#3C3C3C"))+
  theme(legend.position = "bottom")

chi_grafico2


chi_grafico3 <- ggplot(NULL, aes(x = x, colour = df)) +
  labs(title = 'Distribuciones chi cuadrado (teóricas) con distintos grados de libertad',  x ='', y = '') +
  stat_function(data =  data.frame(x = c(0, 40), df = factor(2, levels = niveles)), fun = dchisq, args = list(df = 2)) +
  stat_function(data =  data.frame(x = c(0, 40), df = factor(5, levels = niveles)), fun = dchisq, args = list(df = 5)) +
  stat_function(data =  data.frame(x = c(0, 40), df = factor(10, levels = niveles)), fun = dchisq, args = list(df = 10)) +
  stat_function(data =  data.frame(x = c(0, 40), df = factor(20, levels = niveles)), fun = dchisq, args = list(df = 20)) +
  theme(plot.title=element_text(hjust=0.5, vjust=2, colour="#3C3C3C")) +
  theme(legend.position = "bottom")

chi_grafico3

library(patchwork)

chi_grafico2 / chi_grafico3
