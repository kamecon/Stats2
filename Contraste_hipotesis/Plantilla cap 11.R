
# Dos medias, muestras independientes, varianzas poblacionales conocidas --------

# Cola superior

n_x <-
n_y <- 

media_x <- 
media_y <- 

sigma_x <- 
sigma_y <- 

diferencia_medias <- media_x - media_y

dt <- sqrt((sigma_x/n_x)+(sigma_y/n_y))

alfa <- 
nula <- 0
valor <- (diferencia_medias - nula) / dt
critico <- qnorm(p = alfa, lower.tail = FALSE)
pvalor <- pnorm(valor, lower.tail = FALSE)

# Cola inferior

n_x <-
n_y <- 
  
media_x <- 
media_y <- 
  
sigma_x <- 
sigma_y <- 
  
diferencia_medias <- media_x - media_y

dt <- sqrt((sigma_x/n_x)+(sigma_y/n_y))

alfa <- 
nula <- 0
valor <- (diferencia_medias - nula) / dt
critico <- qnorm(p = alfa)
pvalor <- pnorm(valor)

# Dos colas

n_x <-
n_y <- 
  
media_x <- 
media_y <- 
  
sigma_x <- 
sigma_y <- 
  
diferencia_medias <- media_x - media_y

dt <- sqrt((sigma_x/n_x)+(sigma_y/n_y))

alfa <- /2
nula <- 0
valor <- (diferencia_medias - nula) / dt
critico <- qnorm(p = alfa, lower.tail = FALSE)
pvalor <- pnorm(abs(valor), lower.tail = FALSE)*2


# Dos medias, poblaciones independientes, varianzas desconocidas que se supone que son iguales --------

# Cola superior

n_x <- 
n_y <- 

media_x <- 
media_y <- 

varianza_x <- 
varianza_y <- 


varianza_agrupada <- (((n_x-1)*varianza_x) + ((n_y-1)*varianza_y)) / (n_x + n_y -2)
dt <- sqrt((varianza_agrupada/n_x)+(varianza_agrupada/n_y))

gl <- n_x + n_y -2

diferencia_medias <- media_x - media_y

alfa <- 
nula <- 0
valor <- (diferencia_medias - nula) / dt
critico <- qt(p = alfa, df = gl, lower.tail = FALSE)
pvalor <- pt(valor, df = gl, lower.tail = FALSE)

# Cola inferior

n_x <-
n_y <- 
  
media_x <- 
media_y <- 
  
varianza_x <- 
varianza_y <- 
  
  
varianza_agrupada <- (((n_x-1)*varianza_x) + ((n_y-1)*varianza_y)) / (n_x + n_y -2)
dt <- sqrt((varianza_agrupada/n_x)+(varianza_agrupada/n_y))

gl <- n_x + n_y -2

diferencia_medias <- media_x - media_y

alfa <- 
nula <- 0
valor <- (diferencia_medias - nula) / dt
critico <- qt(p = alfa, df = gl)
pvalor <- pt(valor, df = gl)

# Dos colas

n_x <-
n_y <- 
  
media_x <- 
media_y <- 
  
varianza_x <- 
varianza_y <- 
  
varianza_agrupada <- (((n_x-1)*varianza_x) + ((n_y-1)*varianza_y)) / (n_x + n_y -2)
dt <- sqrt((varianza_agrupada/n_x)+(varianza_agrupada/n_y))

gl <- n_x + n_y -2

diferencia_medias <- media_x - media_y

alfa <- /2
nula <- 0
valor <- (diferencia_medias - nula) / dt
critico <- qt(p = alfa, df = gl, lower.tail = FALSE)
pvalor <- pt(abs(valor), df = gl, lower.tail = FALSE)*2


# Contrastes de la diferencia entre dos proporciones poblacionales --------

# Cola superior

n_a <- 
n_b <- 
  
p_a <- /n_a
p_b <- /n_b

diferencia_p <- p_a - p_b

p_0 <- ((n_a*p_a)+(n_b*p_b))/(n_a + n_b)

dt <- sqrt(((p_0*(1-p_0))/n_a) + ((p_0*(1-p_0))/n_b) )

alfa <- 
nula <- 0
valor <- (diferencia_p - nula) / dt
critico <- qnorm(p = alfa, lower.tail = FALSE)
pvalor <- pnorm(valor, lower.tail = FALSE)

# Cola inferior

n_a <- 
n_b <- 
  
p_a <- /n_a
p_b <- /n_b

diferencia_p <- p_a - p_b

p_0 <- ((n_a*p_a)+(n_b*p_b))/(n_a + n_b)

dt <- sqrt(((p_0*(1-p_0))/n_a) + ((p_0*(1-p_0))/n_b) )

alfa <- 
nula <- 0
valor <- (diferencia_p - nula) / dt
critico <- qnorm(p = alfa)
pvalor <- pnorm(valor)

# Dos colas

n_a <- 
n_b <- 

p_a <- /n_a
p_b <- /n_b

diferencia_p <- p_a - p_b

p_0 <- ((n_a*p_a)+(n_b*p_b))/(n_a + n_b)

dt <- sqrt(((p_0*(1-p_0))/n_a) + ((p_0*(1-p_0))/n_b) )

alfa <- /2
nula <- 0
valor <- (diferencia_p - nula) / dt
critico <- qnorm(p = alfa, lower.tail = FALSE)
pvalor <- pnorm(abs(valor), lower.tail = FALSE)*2 


# Contrastes de hipótesis con datos: la función `t-test` --------

# Estos contrastes se realizan para el caso de varianza desconocida

# Varianzas iguales: cola superior
datos1 <-
datos2 <- 
nula <- 0
significancia <- 
  
t.test(x = datos1, y = datos2, mu = nula, alternative = "greater", var.equal = TRUE, conf.level = 1-significancia)

# Varianzas iguales: cola inferior
datos1 <-
datos2 <- 
nula <- 0
significancia <- 
  
t.test(x = datos1, y = datos2, mu = nula, alternative = "less", var.equal = TRUE, conf.level = 1-significancia)

# Varianzas iguales: dos colas 
datos1 <-  
datos2 <- 
nula <- 0
significancia <- 
  
t.test(x = datos1, y = datos2, mu = nula, alternative = "two.sided", var.equal = TRUE, conf.level = 1-significancia)


# Varianzas no iguales: cola superior
datos1 <-
datos2 <- 
nula <- 0
significancia <- 
  
t.test(x = datos, y = datos2, mu = nula, alternative = "greater", var.equal = FALSE, conf.level = 1-significancia)

# Varianzas no iguales: cola inferior
datos1 <- 
datos2 <- 
nula <- 0
significancia <- 
  
t.test(x = datos, y = datos2, mu = nula, alternative = "less", var.equal = FALSE, conf.level = 1-significancia)

# Varianzas no iguales: dos colas 
datos1 <- 
datos2 <- 
nula <- 0
significancia <- 
  
t.test(x = datos, y = datos2, mu = nula, alternative = "two.sided", var.equal = FALSE, conf.level = 1-significancia)
