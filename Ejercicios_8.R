
# Distribuciones dependientes (pareadas) ----------------------------------

# 9.4 Se elige una muestra aleatoria de 10 pares de viviendas idénticas de una gran ciudad y se instala un sistema pasivo de calefacción solar en uno de los miembros de cada par. Se obtienen las facturas totales de combustible (en dólares) de tres meses de invierno de estas casas que se muestran en la tabla adjunta. Suponiendo que las poblaciones siguen una distribución normal, halle el intervalo de confianza al 90 por ciento de la diferencia entre las dos medias poblacionales.

# Cargamos los datos 
datos <- read.csv("/cloud/project/Clases/9_4.csv", sep="")
datos

# Creamos una columna de diferencias de las facturas
datos$Diferencia <- datos[,2] - datos[,3]
datos

media <- mean(datos$Diferencia)
dt <- sd(datos$Diferencia)

n <- nrow(datos)
confianza <- 0.9
confianza2 <- (1-confianza)/2
tabla <- qt(p = confianza2, df = n-1, lower.tail = FALSE)

ME <- tabla*dt/sqrt(n)

LI <- media - ME
LS <- media + ME

LI
LS


# Muestras independientes, varianzas poblacionales conocidas --------------

# 9.3 El muestreo aleatorio independiente de de dos poblaciones que siguen una distribución normal da los siguientes resultados: 

n_x <- 64
n_y <- 36

media_x <- 400
media_y <- 360

sigma_x <- 20
sigma_y <- 25

# Halle una estimación del intervalo de confianza al 90 por ciento de la diferencia entre las medias de las dos poblaciones

diferencia_medias <- media_x - media_y

confianza <- 0.9
confianza2 <- (1-confianza)/2
tabla <- qnorm(p = confianza2, lower.tail = FALSE)

ME <- tabla*sqrt((sigma_x^2/n_x)+(sigma_y^2/n_y))

LI <- diferencia_medias - ME
LS <- diferencia_medias + ME

LI
LS


# Diferencia entre las medias de dos poblaciones normales cuando las varianzas poblacionales son desconocidas e iguales --------

# En este caso tenemos que utilizar la varianza agrupada cuando calculemos el margen de error

# Resolvemos el ejemplo 9.3

datos <- read.csv("/cloud/project/Clases/ejemplo9_3.csv", sep="")
datos

# Calculamos el tamaño de la muestra como la cantidad de valores disponibles (que no son NA's)

n_x <- sum(!is.na(datos$Orange_City))
n_y <- sum(!is.na(datos$DeLand))

# Cuando calculamos la media y la varianza tomamos en cuenta la existencia de datos faltantes (NA's), por eso colocamos el argumento `na.rm = TRUE`

media_x <- mean(datos$Orange_City, na.rm = TRUE)
media_y <- mean(datos$DeLand, na.rm = TRUE)

varianza_x <- var(datos$Orange_City, na.rm = TRUE)
varianza_y <- var(datos$DeLand, na.rm = TRUE)

varianza_agrupada <- (((n_x-1)*varianza_x) + ((n_y-1)*varianza_y)) / (n_x + n_y -2)

# Calculamos la diferencia de medias

diferencia <- media_x - media_y

# Los grados de libertad 

gl <- n_x + n_y -2

# Y buscamos el valor en la tabla t dado el nivel de confianza

confianza <- 0.95
confianza2 <- (1-confianza)/2
tabla <- qt(p = confianza2, df = gl, lower.tail = FALSE)

# Con esta información procedemos a calcular el intervalo de confianza

ME <- tabla*sqrt((varianza_agrupada/n_x)+(varianza_agrupada/n_y))

LI <- diferencia - ME
LS <- diferencia + ME

LI
LS


# Muestras independientes, varianzas poblacionales desconocidas que no sean iguales --------

# En este caso tenemos que calcular los grados de libertad empleando la expresión 9.10 de la página 334

# Resolvemos el ejemplo 9.4

n_x <- 16
n_y <- 11

media_x <- 290
media_y <- 250

sigma_x <- 15
sigma_y <- 50

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


# Intervalos de confianza de la diferencia entre dos proporciones  --------

# Resolvemos el ejemplo 9.5

n_a <- 120
n_b <- 141

p_a <- 107/n_a
p_b <- 73/n_b

# Calculamos la diferencia de proporciones

diferencia_p <- p_a - p_b


# Buscamos el valor en la tabla z dado el nivel de confianza

confianza <- 0.95
confianza2 <- (1-confianza)/2
tabla <- qnorm(p = confianza2, lower.tail = FALSE)

# Calculamos el intervalo de confianza utilizando la desviación típica de la proporción

ME <- tabla*sqrt(((p_a*(1-p_a))/n_a) + ((p_b*(1-p_b))/n_b) )

LI <- diferencia_p - ME
LS <- diferencia_p + ME

LI
LS



# Intervalos de confianza de la varianza de una distribución normal -------

# En este caso debemos usar la distribución chi- cuadrado

# Resolvemos el ejemplo 9.6

n <- 25
varianza <- 100
confianza <- 0.95
confianza2 <- (1-confianza)/2

tabla_sup <- qchisq(p = 1-confianza2, df = n-1)
tabla_inf <- qchisq(p = confianza2, df = n-1)

LI <- (((n-1)*varianza) / tabla_inf) 
LS <- (((n-1)*varianza) / tabla_sup) 

LI
LS
