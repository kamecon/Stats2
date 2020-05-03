z_estandar <-  function(x, media, ds, n){
  z <- (x - media)/(ds / sqrt(n))
  z
}


# Contrastes de la media de una distribución normal: varianza poblacional conocida --------

# Ejemplo 10.1
media <- 80 
dt <- 8
significancia <- 0.05

n <- 25
media_muestral <- 83

# Una cola
confianza2 <- significancia
tabla <- qnorm(p = confianza2, lower.tail = FALSE)
valor <- z_estandar(x = media_muestral, media = media, ds = dt, n = n)
critico <- media + tabla*(dt/sqrt(n))
pvalor <- pnorm(valor, lower.tail = FALSE)

#Mostramos los datos en una tabla

tabla <- data.frame(
  "Concepto" = c("Valor", "Límite", "Media muestral", "Valor crítico", "p-valor", "significancia"),
  "Datos"=c(valor, tabla, media_muestral, critico, pvalor, significancia)
)

tabla$Datos <-  round(tabla$Datos, digits = 4)

tabla

# Ejemplo 10.2

media <- 5
dt <- 0.1
significancia <- 0.05

n <- 16
media_muestral <- 4.962

# Una cola (compuesta)
confianza2 <- significancia
tabla <- qnorm(p = confianza2, lower.tail = FALSE)*(-1)
valor <- z_estandar(x = media_muestral, media = media, ds = dt, n = n)
critico <- media + tabla*(dt/sqrt(n))
pvalor <- pnorm(valor)

#Mostramos los datos en una tabla

tabla <- data.frame(
  "Concepto" = c("Valor", "Límite", "Media muestral", "Valor crítico", "p-valor", "significancia"),
  "Datos"=c(valor, tabla, media_muestral, critico, pvalor, significancia)
)

tabla$Datos <-  round(tabla$Datos, digits = 4)

tabla

# Ejemplo 10.3

media <- 2
dt <- 0.06
significancia <- 0.05

n <- 9
media_muestral <- 1.95

# Dos colas
confianza2 <- significancia/2
tabla <- qnorm(p = confianza2, lower.tail = FALSE)
valor <- z_estandar(x = media_muestral, media = media, ds = dt, n = n)
critico1 <- media + tabla*(dt/sqrt(n))
critico2 <- media - tabla*(dt/sqrt(n))
pvalor <- pnorm(valor)*2

#Mostramos los datos en una tabla

tabla <- data.frame(
  "Concepto" = c("Valor", "Límite inferior", "Límite superior", "Media muestral", "Valor crítico superior", "Valor crítico inferior", "p-valor", "significancia"),
  "Datos"=c(valor, tabla*(-1), tabla, media_muestral, critico1, critico2, pvalor, (significancia/2))
)

tabla$Datos <-  round(tabla$Datos, digits = 4)

tabla

# Contrastes de la media de una distribución normal: varianza poblacional desconocida --------

# Ejemplo 10.4
media <- 2400 
dt <- 4919
significancia <- 0.05

n <- 134
media_muestral <- 3593

# Una cola
confianza2 <- significancia
tabla <- qt(p = confianza2, df = n-1, lower.tail = FALSE)
valor <- z_estandar(x = media_muestral, media = media, ds = dt, n = n)
critico <- media + tabla*(dt/sqrt(n))
pvalor <- pt(valor,df = n-1, lower.tail = FALSE)

#Mostramos los datos en una tabla

tabla <- data.frame(
  "Concepto" = c("Valor", "Límite", "Media muestral", "Valor crítico", "p-valor", "significancia"),
  "Datos"=c(valor, tabla, media_muestral, critico, pvalor, significancia)
)

tabla$Datos <-  round(tabla$Datos, digits = 4)

tabla

# Compuesta

media <-
dt <- 
significancia <- 

n <- 
media_muestral <- 

# Una cola (compuesta)
confianza2 <- significancia
tabla <- qt(p = confianza2, df = n-1, lower.tail = FALSE)*(-1)
valor <- z_estandar(x = media_muestral, media = media, ds = dt, n = n)
critico <- media + tabla*(dt/sqrt(n))
pvalor <- pt(valor)

#Mostramos los datos en una tabla

tabla <- data.frame(
  "Concepto" = c("Valor", "Límite", "Media muestral", "Valor crítico", "p-valor", "significancia"),
  "Datos"=c(valor, tabla, media_muestral, critico, pvalor, significancia)
)

tabla$Datos <-  round(tabla$Datos, digits = 4)

tabla

# Dos colas

media <-
dt <- 
significancia <- 

n <- 
media_muestral <- 

# Dos colas
confianza2 <- significancia/2
tabla <- qt(p = confianza2, df = n-1, lower.tail = FALSE)
valor <- z_estandar(x = media_muestral, media = media, ds = dt, n = n)
critico1 <- media + tabla*(dt/sqrt(n))
critico2 <- media - tabla*(dt/sqrt(n))
pvalor <- pt(valor)*2

#Mostramos los datos en una tabla

tabla <- data.frame(
  "Concepto" = c("Valor", "Límite inferior", "Límite superior", "Media muestral", "Valor crítico superior", "Valor crítico inferior", "p-valor", "significancia"),
  "Datos"=c(valor, tabla*(-1), tabla, media_muestral, critico1, critico2, pvalor, (significancia/2))
)

tabla$Datos <-  round(tabla$Datos, digits = 4)

tabla

# Contrastes de la proporción poblacional --------

#Ejemplo 10.5

proporcion_poblacional <- 0.5
n <- 802
proporcion_muestral <- 378/n
dt <- sqrt((proporcion_poblacional*(1-proporcion_poblacional))/n)
significancia <- 0.07

# Una cola (inferior)
confianza2 <- significancia
tabla <- qnorm(p = confianza2, lower.tail = FALSE)*(-1)
valor <- (proporcion_muestral - proporcion_poblacional)/dt
critico <- proporcion_poblacional - tabla*dt
pvalor <- pnorm(valor)

#Mostramos los datos en una tabla

tabla <- data.frame(
  "Concepto" = c("Valor", "Límite", "Media muestral", "Valor crítico", "p-valor", "significancia"),
  "Datos"=c(valor, tabla, proporcion_muestral, critico, pvalor, significancia)
)

tabla$Datos <-  round(tabla$Datos, digits = 4)

tabla
