
# Contrastes de la diferencia entre dos medias poblacionales: datos pareados --------------

# Ejemplo 11.1

datos11_1 <- read.csv("/cloud/project/Clases/11_1.csv", sep="", stringsAsFactors=FALSE)

t.test(x = datos11_1$X_bien_recordado, y = datos11_1$Y_mal_recordado, mu = 0, alternative = "greater", paired = TRUE, conf.level = 0.95 )

qt(p = 0.05, df = 9, lower.tail = FALSE)


# Dos medias, muestras independientes, varianzas poblacionales conocidas --------

# Ejemplo 11.2

n_x <- 25
n_y <- 25

media_x <- 115
media_y <- 100

sigma_x <- 625
sigma_y <- 400

diferencia_medias <- media_x - media_y

dt <- sqrt((sigma_x/n_x)+(sigma_y/n_y))

alfa <- 0.05
nula <- 0
valor <- (diferencia_medias - nula) / dt
critico <- qnorm(p = alfa, lower.tail = FALSE)
pvalor <- pnorm(valor, lower.tail = FALSE)


# Dos medias, poblaciones independientes, varianzas desconocidas que se supone que son iguales --------

# Ejemplo 11.3

n_x <- 25
n_y <- 25

media_x <- 1078
media_y <- 908.2

varianza_x <- 633^2
varianza_y <- 469.8^2


varianza_agrupada <- (((n_x-1)*varianza_x) + ((n_y-1)*varianza_y)) / (n_x + n_y -2)
dt <- sqrt((varianza_agrupada/n_x)+(varianza_agrupada/n_y))

# Los grados de libertad 

gl <- n_x + n_y -2

# Calculamos la diferencia de medias

diferencia_medias <- media_x - media_y

alfa <- 0.05
nula <- 0
valor <- (diferencia_medias - nula) / dt
critico <- qt(p = alfa, df = gl, lower.tail = FALSE)
pvalor <- pt(valor, df = gl, lower.tail = FALSE)

# Ejemplo 11.4

t.test(x = datos11_1$X_bien_recordado, y = datos11_1$Y_mal_recordado, mu = 0, alternative = "greater", var.equal = TRUE, conf.level = 0.95 )

qt(p = 0.05, df = 18, lower.tail = FALSE)


# Dos medias, muestras independientes, varianzas poblacionales desconocidas que se supone que no son iguales --------

# Figura 11.3

t.test(x = datos11_1$X_bien_recordado, y = datos11_1$Y_mal_recordado, mu = 0, alternative = "greater", var.equal = FALSE, conf.level = 0.95 )

qt(p = 0.05, df = 15, lower.tail = FALSE)



# Contrastes de la diferencia entre dos proporciones poblacionales --------

# Ejemplo 11.5

p_a <- 
p_b <- 
  
n_a <- 203
n_b <- 270

p_a <- 52/n_a
p_b <- 56/n_b

# Calculamos la diferencia de proporciones
  
diferencia_p <- p_a - p_b

# Calculamos la proporción desconocida

p_0 <- ((n_a*p_a)+(n_b*p_b))/(n_a + n_b)

# Calculamos la desviación típica

dt <- sqrt(((p_0*(1-p_0))/n_a) + ((p_0*(1-p_0))/n_b) )

# Como es el caso de una hipótesis bilateral debemos dividir alfa entre 2 (HACER ESTO EN CADA CASO SI SE LES PIDE UN CONTRASTE BILATERAL -DOS COLAS-)
alfa <- 0.10
alfa2 <- alfa/2
nula <- 0
valor <- (diferencia_p - nula) / dt
critico <- qnorm(p = alfa2, lower.tail = FALSE)
pvalor <- pnorm(valor, lower.tail = FALSE)
