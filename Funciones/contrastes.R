
contraste_media1(media = 2400, dt = 4919, significancia = 0.05, n = 134, tipo = "superior", media_muestral = 3593)


contraste_media1 <- function(media, dt=NULL, varianza=NULL, significancia, n, media_muestral, tipo="superior"){

# Contrastes de la media de una distribución normal: varianza poblacional conocida 

  if(!is.null(varianza)){
    
    z_estandar <-  (media_muestral - media)/(varianza / sqrt(n))
  
# Contraste de cola superior

  if(tipo=="superior") {

confianza2 <- significancia
tabla <- qnorm(p = confianza2, lower.tail = FALSE)
valor <- z_estandar
critico <- media + tabla*(varianza/sqrt(n))
pvalor <- pnorm(valor, lower.tail = FALSE)

  }
  
# Contraste de cola inferior
  
  if(tipo=="inferior") {

confianza2 <- significancia
tabla <- qnorm(p = confianza2, lower.tail = FALSE)*(-1)
valor <- z_estandar
critico <- media + tabla*(varianza/sqrt(n))
pvalor <- pnorm(valor)

  }
  
# Contraste de dos colas
  
  if(tipo=="bilateral") {
  
confianza2 <- significancia/2
tabla <- qnorm(p = confianza2, lower.tail = FALSE)
valor <- z_estandar
critico1 <- media + tabla*(varianza/sqrt(n))
critico2 <- media - tabla*(varianza/sqrt(n))
pvalor <- pnorm(abs(valor), lower.tail = FALSE)*2

  }
  
  }
  
# Contrastes de la media de una distribución normal: varianza poblacional desconocida --------

  if(!is.null(dt)){
    
    z_estandar <-  (media_muestral - media)/(dt / sqrt(n))
  
# Contraste de cola superior
  if(tipo=="superior") {

confianza2 <- significancia
tabla <- qt(p = confianza2, df = n-1, lower.tail = FALSE)
valor <- z_estandar
critico <- media + tabla*(dt/sqrt(n))
pvalor <- pt(valor,df = n-1, lower.tail = FALSE)

  }
  
# Contraste de cola inferior
  if(tipo=="inferior") {

confianza2 <- signiicancia
tabla <- qt(p = confianza2, df = n-1, lower.tail = FALSE)*(-1)
valor <- z_estandar
critico <- media + tabla*(dt/sqrt(n))
pvalor <- pt(valor, df = n-1)

  }
  
# Contraste de dos colas
  if(tipo=="bilateral") {

confianza2 <- significancia/2
tabla <- qt(p = confianza2, df = n-1, lower.tail = FALSE)
valor <- z_estandar
critico1 <- media + tabla*(dt/sqrt(n))
critico2 <- media - tabla*(dt/sqrt(n))
pvalor <- pt(abs(valor), df = n-1, lower.tail = FALSE)*2

  }
  
  }
  
  if(tipo=="bilateral") {
    
    tabla <- data.frame(
      "Concepto" = c("Valor", "Valor crítico superior", "Valor crítico inferior", "Media muestral", "Límite inferior", "Límite superior",  "p-valor", "significancia"),
      "Datos"=c(valor, tabla*(-1), tabla, media_muestral, critico1, critico2, pvalor, (significancia/2))
    )
    
    tabla$Datos <-  round(tabla$Datos, digits = 4)
    
    tabla
    
  } else {
  
  tabla <- data.frame(
    "Concepto" = c("Estadistico contraste", "Valor crítico", "Media muestral", "Limite", "p-valor", "significancia"),
    "Datos"=c(valor, tabla, media_muestral, critico, pvalor, significancia)
  )
  
  tabla$Datos <-  round(tabla$Datos, digits = 4)
  
  tabla
  
  }
}


# Contrastes de la proporción poblacional --------

# Contraste de cola superior

proporcion_poblacional <- 0.023
  n <- 200
  proporcion_muestral <- .041
dt <- sqrt((proporcion_poblacional*(1-proporcion_poblacional))/n)
significancia <- 0.05
  
  confianza2 <- significancia
tabla_z <- qnorm(p = confianza2, lower.tail = FALSE)
valor <- (proporcion_muestral - proporcion_poblacional)/dt
critico <- proporcion_poblacional - tabla_z*dt
pvalor <- pnorm(valor, lower.tail = FALSE)


tabla <- data.frame(
  "Concepto" = c("Valor", "Límite", "Media muestral", "Valor crítico", "p-valor", "significancia"),
  "Datos"=c(valor, tabla_z, proporcion_muestral, critico, pvalor, significancia)
)

tabla$Datos <-  round(tabla$Datos, digits = 4)

tabla

# Contraste de cola inferior

proporcion_poblacional <- 0.023
  n <- 200
  proporcion_muestral <- .041
dt <- sqrt((proporcion_poblacional*(1-proporcion_poblacional))/n)
significancia <- 0.05
  
  confianza2 <- significancia
tabla_z <- qnorm(p = confianza2, lower.tail = FALSE)*(-1)
valor <- (proporcion_muestral - proporcion_poblacional)/dt
critico <- proporcion_poblacional - tabla_z*dt
pvalor <- pnorm(valor)


tabla <- data.frame(
  "Concepto" = c("Valor", "Límite", "Media muestral", "Valor crítico", "p-valor", "significancia"),
  "Datos"=c(valor, tabla_z, proporcion_muestral, critico, pvalor, significancia)
)

tabla$Datos <-  round(tabla$Datos, digits = 4)

tabla

# Contraste de dos colas

proporcion_poblacional <-
  n <- 
  proporcion_muestral <- /n
dt <- sqrt((proporcion_poblacional*(1-proporcion_poblacional))/n)
significancia <- 
  
  confianza2 <- significancia/2
tabla_z <- qnorm(p = confianza2, lower.tail = FALSE)
valor <- (proporcion_muestral - proporcion_poblacional)/dt
critico <- proporcion_poblacional - tabla_z*dt
pvalor <- pnorm(abs(valor), lower.tail = FALSE)*2


tabla <- data.frame(
  "Concepto" = c("Valor", "Límite", "Media muestral", "Valor crítico", "p-valor", "significancia"),
  "Datos"=c(valor, tabla_z, proporcion_muestral, critico, pvalor, significancia)
)

tabla$Datos <-  round(tabla$Datos, digits = 4)

tabla



# Contrastes de hipótesis con datos: la función `t-test` --------

# Estos contrastes se realizan para el caso de varianza desconocida

# Varianzas iguales: cola superior
datos <- 
  media <- 
  dt <- 
  significancia <- 
  
  t.test(x = datos, mu = media, alternative = "greater", var.equal = TRUE, conf.level = 1-significancia)

# Varianzas iguales: cola inferior
datos <- 
  media <- 
  dt <- 
  significancia <- 
  
  t.test(x = datos, mu = media, alternative = "less", var.equal = TRUE, conf.level = 1-significancia)

# Varianzas iguales: dos colas 
datos <- 
  media <- 
  dt <- 
  significancia <- 
  
  t.test(x = datos, mu = media, alternative = "two.sided", var.equal = TRUE, conf.level = 1-significancia)

# Varianzas no iguales: cola superior
datos <- 
  media <- 
  dt <- 
  significancia <- 
  
  t.test(x = datos, mu = media, alternative = "greater", var.equal = TRUE, conf.level = 1-significancia)

# Varianzas no iguales: cola inferior
datos <- 
  media <- 
  dt <- 
  significancia <- 
  
  t.test(x = datos, mu = media, alternative = "less", var.equal = TRUE, conf.level = 1-significancia)

# Varianzas no iguales: dos colas 
datos <- 
  media <- 
  dt <- 
  significancia <- 
  
  t.test(x = datos, mu = media, alternative = "two.sided", var.equal = FALSE, conf.level = 1-significancia)


# Dos medias, muestras independientes, varianzas poblacionales conocidas --------

# Cola superior

n_x <- 97
  n_y <- 83
  
  media_x <- 8.7
  media_y <- 8.2
  
  sigma_x <- 1
  sigma_y <- 1.3
  
  diferencia_medias <- media_x - media_y

dt <- sqrt((sigma_x/n_x)+(sigma_y/n_y))

alfa <- 0.07
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
