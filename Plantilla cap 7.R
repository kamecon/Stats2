
#Función que estandariza valores
z_estandar <-  function(x, media, ds, n){
  z <- (x - media)/(ds / sqrt(n))
  z
  }

# Área entre dos valores ----

# Media
media <- 
# Valor mayor
x_1 <- 
# Valor menor
x_2 <- 
# Desviación estándar
dt <- 
# Tamaño de la muestra
n <- 

z_derecha <-  z_estandar(x_1 = , media = media, ds = dt, n = n)
z_izquierda <-  z_estandar(x_2 = , media = media, ds = dt, n = n)

prob_dos <- pnorm(z_derecha) - pnorm(z_izquierda)

prob_dos



# Área a la izquierda ----

# Media
media <- 
# Valor mayor
x <- 
# Desviación estándar
dt <- 
# Tamaño de la muestra
n <- 
  
z <-  z_estandar(x = x, media = media, ds = dt, n = n)

prob_izq <- pnorm(z)

# Área a la derecha ----

# Media
media <- 
# Valor mayor
x <- 
# Desviación estándar
dt <- 
# Tamaño de la muestra
n <- 
  
z <-  z_estandar(x = x, media = media, ds = dt, n = n)

prob_der <- pnorm(z_7.8, lower.tail = FALSE)

prob_der


#Distribución chi cuadrado

#Búsqueda de tabla básica. Valor de la variable tal que el área a la derecha contenga una determinada probabilidad
#En este ejemplo, el valor de la variable aleatoria tal que el área a la derecha contenga el 5% de probabilidad en una chi cuadrado con 17 grados de libertad

n=18

qchisq(0.05,df = n-1, lower.tail = FALSE)

probchi_1 <-  round(qchisq(0.05,df = n-1, lower.tail = FALSE), digits = 2)

probchi_1

#Valor de la variable tal que el área a la izquierda contenga una determinada probabilidad
#En este ejemplo, el valor de la variable aleatoria tal que el área a la izquierda contenga el 5% de probabilidad en una chi cuadrado con 17 grados de libertad

n=18

qchisq(0.05,df = n-1)

probchi_2 <-  round(qchisq(0.05,df = n-1), digits = 2)

probchi_2

#Probabilidad en el área a la derecha de un determinado valor
#En este ejemplo, probabilidad en el área a la derecha del valor 36 en una chi cuadrado con 15 grados de libertad

pchisq(27,df = 15, lower.tail = FALSE)

probchi_3 <-  round(pchisq(27,df = 15, lower.tail = FALSE), digits = 2)

probchi_3

#El siguiente ejemplo es para resolver problemas como el del ejercicio 7.47

#Se obtiene una muestra aleatoria de tamaño n=16 de una población que sigue una distribución normal de media k=100 y varianza igual a 25. ¿Cuál es la probabilidad de que la varianza muestral sea superior a 45?

# Solución: P((n-1)s^2 / sigma^2 > 15(45) / 25) = P((n-1)s^2 / sigma^2 > 27)

pchisq(27,df = 15, lower.tail = FALSE) #colocamos lower tail=FALSE porque la pregunta nos pide que sea mayor a 45
