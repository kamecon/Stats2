# 9.11 -------

n_x <- 6
n_y <- 9

media_x <- 76.12
media_y <- 74.61

sigma_x <- 2.53
sigma_y <- 8.61

# Calculamos la diferencia de medias

diferencia <- media_x - media_y

# Calculamos los grados de libertad

gl <- (((sigma_x^2/n_x) + (sigma_y^2/n_y))^2) / (((1/(n_x - 1))*(sigma_x^2/n_x)^2) + ((1/(n_y - 1))*(sigma_y^2/n_y)^2))

gl_redondo <- round(gl)

# Y buscamos el valor en la tabla t dado el nivel de confianza

confianza <- 0.95
confianza2 <- (1-confianza)/2
tabla <- qt(p = confianza2, df = gl_redondo, lower.tail = FALSE)

# Con esta información procedemos a calcular el intervalo de confianza

ME <- tabla*sqrt((sigma_x^2/n_x)+(sigma_y^2/n_y))

LI <- diferencia - ME
LS <- diferencia + ME

LI
LS

# 9.13 -------

n_x <- 16
n_y <- 9

media_x <- 34500
media_y <- 31499

varianza_x <- 8520^2
varianza_y <- 7521^2


varianza_agrupada <- (((n_x-1)*varianza_x) + ((n_y-1)*varianza_y)) / (n_x + n_y -2)

# Los grados de libertad 

gl <- n_x + n_y -2

# Calculamos la diferencia de medias

diferencia <- media_x - media_y


# Y buscamos el valor en la tabla t dado el nivel de confianza

confianza <- 0.9
confianza2 <- (1-confianza)/2
tabla <- qt(p = confianza2, df = gl, lower.tail = FALSE)

# Con esta información procedemos a calcular el intervalo de confianza

ME <- tabla*sqrt((varianza_agrupada/n_x)+(varianza_agrupada/n_y))

LI <- diferencia - ME
LS <- diferencia + ME

LI
LS

# 9.15 -------

n_x <- 9
n_y <- 10

media_x <- 9.78
media_y <- 15.10

varianza_x <- 17.64
varianza_y <- 27.01


varianza_agrupada <- (((n_x-1)*varianza_x) + ((n_y-1)*varianza_y)) / (n_x + n_y -2)

# Los grados de libertad 

gl <- n_x + n_y -2

# Calculamos la diferencia de medias

diferencia <- media_x - media_y


# Y buscamos el valor en la tabla t dado el nivel de confianza

confianza <- 0.9
confianza2 <- (1-confianza)/2
tabla <- qt(p = confianza2, df = gl, lower.tail = FALSE)

# Con esta información procedemos a calcular el intervalo de confianza

ME <- tabla*sqrt((varianza_agrupada/n_x)+(varianza_agrupada/n_y))

LI <- diferencia - ME
LS <- diferencia + ME

LI
LS

# 9.29 -------

n <- 18
varianza <- 10.4^2
confianza <- 0.9
confianza2 <- (1-confianza)/2

tabla_inf <- qchisq(p = 1-confianza2, df = n-1)
tabla_sup <- qchisq(p = confianza2, df = n-1)

LI <- (((n-1)*varianza) / tabla_inf) 
LS <- (((n-1)*varianza) / tabla_sup) 

LI
LS

# 9.39 -------

n_x <- 15
n_y <- 13

media_x <- 400
media_y <- 360

sigma_x <- 10
sigma_y <- 40

# Calculamos la diferencia de medias

diferencia <- media_x - media_y

# Calculamos los grados de libertad

gl <- (((sigma_x^2/n_x) + (sigma_y^2/n_y))^2) / (((1/(n_x - 1))*(sigma_x^2/n_x)^2) + ((1/(n_y - 1))*(sigma_y^2/n_y)^2))

gl_redondo <- round(gl)

# Y buscamos el valor en la tabla t dado el nivel de confianza

confianza <- 0.90
confianza2 <- (1-confianza)/2
tabla <- qt(p = confianza2, df = gl_redondo, lower.tail = FALSE)

# Con esta información procedemos a calcular el intervalo de confianza

ME <- tabla*sqrt((sigma_x^2/n_x)+(sigma_y^2/n_y))

LI <- diferencia - ME
LS <- diferencia + ME

LI
LS

# 9.41 -------

n_x <- 16
n_y <- 10

media_x <- 625000
media_y <- 608000

varianza_x <- 80000^2
varianza_y <- 73000^2


varianza_agrupada <- (((n_x-1)*varianza_x) + ((n_y-1)*varianza_y)) / (n_x + n_y -2)

# Los grados de libertad 

gl <- n_x + n_y -2

# Calculamos la diferencia de medias

diferencia <- media_x - media_y


# Y buscamos el valor en la tabla t dado el nivel de confianza

confianza <- 0.9
confianza2 <- (1-confianza)/2
tabla <- qt(p = confianza2, df = gl, lower.tail = FALSE)

# Con esta información procedemos a calcular el intervalo de confianza

ME <- tabla*sqrt((varianza_agrupada/n_x)+(varianza_agrupada/n_y))

LI <- diferencia - ME
LS <- diferencia + ME

LI
LS

# 9.45 -------

n_x <- 40
n_y <- 50

media_x <- 340
media_y <- 285

varianza_x <- 20^2
varianza_y <- 30^2


varianza_agrupada <- (((n_x-1)*varianza_x) + ((n_y-1)*varianza_y)) / (n_x + n_y -2)

# Los grados de libertad 

gl <- n_x + n_y -2

# Calculamos la diferencia de medias

diferencia <- media_x - media_y


# Y buscamos el valor en la tabla t dado el nivel de confianza

confianza <- 0.9
confianza2 <- (1-confianza)/2
tabla <- qt(p = confianza2, df = gl, lower.tail = FALSE)

# Con esta información procedemos a calcular el intervalo de confianza

ME <- tabla*sqrt((varianza_agrupada/n_x)+(varianza_agrupada/n_y))

LI <- diferencia - ME
LS <- diferencia + ME

LI
LS
