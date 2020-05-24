
# Dos medias, muestras independientes, varianzas poblacionales conocidas --------

n_x <- 
n_y <- 
  
media_x <- 
media_y <- 
  
# Acá deben introducir LA VARIANZA
varianza_x <- 
varianza_y <- 
  
# Calculamos la diferencia de medias

diferencia <- media_x - media_y

# Y buscamos el valor en la tabla t dado el nivel de confianza

confianza <- 
confianza2 <- (1-confianza)/2
tabla <- qnorm(p = confianza2, lower.tail = FALSE)

# Con esta información procedemos a calcular el intervalo de confianza

ME <- tabla*sqrt((varianza_x/n_x)+(varianza_y/n_y))

LI <- diferencia - ME
LS <- diferencia + ME

LI
LS



# Dos medias, poblaciones independientes, varianzas desconocidas que se supone que son iguales --------

n_x <- 
n_y <- 

media_x <- 
media_y <- 

# Acá deben introducir LA VARIANZA
varianza_x <- 
varianza_y <- 


varianza_agrupada <- (((n_x-1)*varianza_x) + ((n_y-1)*varianza_y)) / (n_x + n_y -2)

# Los grados de libertad 

gl <- n_x + n_y -2

# Calculamos la diferencia de medias

diferencia <- media_x - media_y


# Y buscamos el valor en la tabla t dado el nivel de confianza

confianza <- 
confianza2 <- (1-confianza)/2
tabla <- qt(p = confianza2, df = gl, lower.tail = FALSE)

# Con esta información procedemos a calcular el intervalo de confianza

ME <- tabla*sqrt((varianza_agrupada/n_x)+(varianza_agrupada/n_y))

LI <- diferencia - ME
LS <- diferencia + ME

LI
LS


# Dos medias, poblaciones independientes, varianzas desconocidas que se supone que son distintas --------

n_x <- 
n_y <- 

media_x <- 
media_y <- 

# Acá deben introducir la DESVIACIÓN ESTANDAR
sigma_x <- 
sigma_y <- 

# Calculamos la diferencia de medias

diferencia <- media_x - media_y

# Calculamos los grados de libertad

gl <- (((sigma_x^2/n_x) + (sigma_y^2/n_y))^2) / (((1/(n_x - 1))*(sigma_x^2/n_x)^2) + ((1/(n_y - 1))*(sigma_y^2/n_y)^2))

gl_redondo <- round(gl)

# Y buscamos el valor en la tabla t dado el nivel de confianza

confianza <- 
confianza2 <- (1-confianza)/2
tabla <- qt(p = confianza2, df = gl_redondo, lower.tail = FALSE)

# Con esta información procedemos a calcular el intervalo de confianza

ME <- tabla*sqrt((sigma_x^2/n_x)+(sigma_y^2/n_y))

LI <- diferencia - ME
LS <- diferencia + ME

LI
LS


# Contrastes de la diferencia entre dos proporciones poblacionales --------

n_a <- 
n_b <- 

p_a <- /n_a
p_b <- /n_b

diferencia_p <- p_a - p_b

# Calculamos la desviación típica

dt <- sqrt(((p_a*(1-p_a))/n_a) + ((p_b*(1-p_b))/n_b) )

confianza <- 
confianza2 <- (1 - confianza)/2

tabla <- qnorm(p = confianza2, lower.tail = FALSE)

ME <- tabla*dt

LI <- diferencia_p - ME
LS <- diferencia_p + ME

LI
LS

# Intervalos de confianza de la varianza de una distribución normal --------

n <- 
varianza <- 
confianza <- 
confianza2 <- (1-confianza)/2

tabla_inf <- qchisq(p = confianza2, df = n-1)
tabla_sup <- qchisq(p = confianza2, df = n-1,lower.tail = FALSE)

LS <- (((n-1)*varianza) / tabla_inf) 
LI <- (((n-1)*varianza) / tabla_sup) 

LI
LS

