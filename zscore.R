library(ggplot2)

#Función que estandariza valores
z_estandar <-  function(x, media, ds, n){
  z <- (x - media)/(ds / sqrt(n))
  z
  }

#Ejemplo área entre dos valores
z_8.2 <-  z_estandar(x = 8.2, media = 8, ds = 3, n = 36)
z_7.8 <-  z_estandar(x = 7.8, media = 8, ds = 3, n = 36)

z_7.8
z_8.2

prob_dos <- pnorm(z_8.2) - pnorm(z_7.8)

prob_dos

round(prob, digits = 4)

ggplot(data.frame(x=c(-4,4)), aes(x = x)) +
  stat_function(fun = dnorm) +
  stat_function(fun = dnorm, xlim = c(z_7.8, z_8.2), geom = "area", fill = "turquoise2", alpha = 0.8)  +
annotate("text", x = mean(c(z_7.8,z_8.2)), y = 0.15, label = as.character(round(prob_dos, digits = 2)))


#Ejemplo área a la izquierda
z_7.8 <-  z_estandar(x = 7.8, media = 8, ds = 3, n = 36)

z_7.8

prob_izq <- pnorm(z_7.8)

prob_izq

round(prob_izq, digits = 4)

ggplot(data.frame(x=c(-4,4)), aes(x = x)) +
  stat_function(fun = dnorm) +
  stat_function(fun = dnorm, xlim = c(-4, z_7.8), geom = "area", fill = "turquoise2", alpha = 0.8)  +
  annotate("text", x = (z_7.8 + -1.8)/2, y = 0.15, label = as.character(round(prob_izq, digits = 2)))


#Ejemplo área a la derecha
z_7.8 <-  z_estandar(x = 7.8, media = 8, ds = 3, n = 36)

z_7.8

prob_der <- 1 - pnorm(z_7.8)

prob_der

round(prob_der, digits = 4)

ggplot(data.frame(x=c(-4,4)), aes(x = x)) +
  stat_function(fun = dnorm) +
  stat_function(fun = dnorm, xlim = c(z_7.8, 4), geom = "area", fill = "turquoise2", alpha = 0.8) +
  annotate("text", x = (z_7.8 + 1.8)/2, y = 0.15, label = as.character(round(prob_der, digits = 2)))

#Distribución chi cuadrado

#Búsqueda de tabla básica. Valor de la variable tal que el área a la derecha contenga una determinada probabilidad
#En este ejemplo, el valor de la variable aleatoria tal que el área a la derecha contenga el 5% de probabilidad en una chi cuadrado con 17 grados de libertad

n=18

qchisq(0.05,df = n-1, lower.tail = FALSE)

probchi_1 <-  round(qchisq(0.05,df = n-1, lower.tail = FALSE), digits = 2)

ggplot(data.frame(x=c(0,40)), aes(x = x)) +
  stat_function(fun = dchisq, args = list(df = n-1)) +
  stat_function(fun = dchisq, args = list(df = n-1), xlim = c(probchi_1, 40), geom = "area", fill = "turquoise2", alpha = 0.8) +
  geom_vline(xintercept = probchi_1,col="red")

#Valor de la variable tal que el área a la izquierda contenga una determinada probabilidad
#En este ejemplo, el valor de la variable aleatoria tal que el área a la izquierda contenga el 5% de probabilidad en una chi cuadrado con 17 grados de libertad

n=18

qchisq(0.05,df = n-1)

probchi_2 <-  round(qchisq(0.05,df = n-1), digits = 2)

ggplot(data.frame(x=c(0,40)), aes(x = x)) +
  stat_function(fun = dchisq, args = list(df = n-1)) +
  stat_function(fun = dchisq, args = list(df = n-1), xlim = c(0, probchi_2), geom = "area", fill = "turquoise2", alpha = 0.8) +
  geom_vline(xintercept = probchi_2,col="red")

#Probabilidad en el área a la derecha de un determinado valor
#En este ejemplo, probabilidad en el área a la derecha del valor 36 en una chi cuadrado con 15 grados de libertad

pchisq(27,df = 15, lower.tail = FALSE)

probchi_3 <-  round(pchisq(27,df = 15, lower.tail = FALSE), digits = 2)

ggplot(data.frame(x=c(0,40)), aes(x = x)) +
  stat_function(fun = dchisq, args = list(df = 15)) +
  stat_function(fun = dchisq, args = list(df = 15), xlim = c(27, 40), geom = "area", fill = "turquoise2", alpha = 0.8) +
  geom_vline(xintercept = 27,col="red")
