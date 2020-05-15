z_estandar <-  function(x, media, ds, n){
  z <- (x - media)/(ds / sqrt(n))
  z
}


# Contrastes de la media de una distribución normal: varianza poblacional conocida --------

# Contraste de cola superior
media <- 
dt <- 
significancia <- 

n <- 
media_muestral <-

confianza2 <- significancia
tabla_z <- qnorm(p = confianza2, lower.tail = FALSE)
valor <- z_estandar(x = media_muestral, media = media, ds = dt, n = n)
critico <- media + tabla_z*(dt/sqrt(n))
pvalor <- pnorm(valor, lower.tail = FALSE)

tabla <- data.frame(
  "Concepto" = c("Valor", "Límite", "Media muestral", "Valor crítico", "p-valor", "significancia"),
  "Datos"=c(valor, tabla_z, media_muestral, critico, pvalor, significancia)
)

tabla$Datos <-  round(tabla$Datos, digits = 4)

tabla

# Contraste de cola inferior

media <- 
dt <- 
significancia <- 

n <- 
media_muestral <- 

confianza2 <- significancia
tabla_z <- qnorm(p = confianza2, lower.tail = FALSE)*(-1)
valor <- z_estandar(x = media_muestral, media = media, ds = dt, n = n)
critico <- media + tabla_z*(dt/sqrt(n))
pvalor <- pnorm(valor)

tabla <- data.frame(
  "Concepto" = c("Valor", "Límite", "Media muestral", "Valor crítico", "p-valor", "significancia"),
  "Datos"=c(valor, tabla_z, media_muestral, critico, pvalor, significancia)
)

tabla$Datos <-  round(tabla$Datos, digits = 4)

tabla

# Contraste de dos colas

media <- 
dt <- 
significancia <- 

n <- 
media_muestral <- 

confianza2 <- significancia/2
tabla_z <- qnorm(p = confianza2, lower.tail = FALSE)
valor <- z_estandar(x = media_muestral, media = media, ds = dt, n = n)
critico1 <- media + tabla_z*(dt/sqrt(n))
critico2 <- media - tabla_z*(dt/sqrt(n))
pvalor <- pnorm(abs(valor), lower.tail = FALSE)*2

tabla <- data.frame(
  "Concepto" = c("Valor", "Límite inferior", "Límite superior", "Media muestral", "Valor crítico superior", "Valor crítico inferior", "p-valor", "significancia"),
  "Datos"=c(valor, tabla_z*(-1), tabla_z, media_muestral, critico1, critico2, pvalor, (significancia/2))
)

tabla$Datos <-  round(tabla$Datos, digits = 4)

tabla

# Contrastes de la media de una distribución normal: varianza poblacional desconocida --------

# Contraste de cola superior
media <-
dt <- 
significancia <- 0.05

n <- 18
media_muestral <- 

confianza2 <- significancia
tabla_t <- qt(p = confianza2, df = n-1, lower.tail = FALSE)
valor <- z_estandar(x = media_muestral, media = media, ds = dt, n = n)
critico <- media + tabla_t*(dt/sqrt(n))
pvalor <- pt(valor,df = n-1, lower.tail = FALSE)

tabla <- data.frame(
  "Concepto" = c("Valor", "Límite", "Media muestral", "Valor crítico", "p-valor", "significancia"),
  "Datos"=c(valor, tabla_t, media_muestral, critico, pvalor, significancia)
)

tabla$Datos <-  round(tabla$Datos, digits = 4)

tabla

# Contraste de cola inferior

media <-
dt <- 
significancia <- 

n <- 
media_muestral <- 

confianza2 <- significancia
tabla_t <- qt(p = confianza2, df = n-1, lower.tail = FALSE)*(-1)
valor <- z_estandar(x = media_muestral, media = media, ds = dt, n = n)
critico <- media + tabla_t*(dt/sqrt(n))
pvalor <- pt(valor, df = n-1)

tabla <- data.frame(
  "Concepto" = c("Valor", "Límite", "Media muestral", "Valor crítico", "p-valor", "significancia"),
  "Datos"=c(valor, tabla_t, media_muestral, critico, pvalor, significancia)
)

tabla$Datos <-  round(tabla$Datos, digits = 4)

tabla

# Contraste de dos colas

media <-
dt <- 
significancia <- 

n <- 
media_muestral <- 

confianza2 <- significancia/2
tabla_t <- qt(p = confianza2, df = n-1, lower.tail = FALSE)
valor <- z_estandar(x = media_muestral, media = media, ds = dt, n = n)
critico1 <- media + tabla_t*(dt/sqrt(n))
critico2 <- media - tabla_t*(dt/sqrt(n))
pvalor <- pt(abs(valor), df = n-1, lower.tail = FALSE)*2


tabla <- data.frame(
  "Concepto" = c("Valor", "Límite inferior", "Límite superior", "Media muestral", "Valor crítico superior", "Valor crítico inferior", "p-valor", "significancia"),
  "Datos"=c(valor, tabla_t*(-1), tabla_t, media_muestral, critico1, critico2, pvalor, (significancia/2))
)

tabla$Datos <-  round(tabla$Datos, digits = 4)

tabla

# Contrastes de la proporción poblacional --------

# Contraste de cola superior

proporcion_poblacional <-
n <- 
proporcion_muestral <- /n
dt <- sqrt((proporcion_poblacional*(1-proporcion_poblacional))/n)
significancia <- 
  
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

proporcion_poblacional <-
n <- 
proporcion_muestral <- /n
dt <- sqrt((proporcion_poblacional*(1-proporcion_poblacional))/n)
significancia <- 

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

# Cola superior
datos <- 
media <- 
significancia <- 
  
t.test(x = datos, mu = media, alternative = "greater", conf.level = 1-significancia)

# Cola inferior
datos <- 
media <-
significancia <- 

t.test(x = datos, mu = media, alternative = "less", conf.level = 1-significancia)

# Dos colas 
datos <- 
media <- 
significancia <- 

t.test(x = datos, mu = media, alternative = "two.sided", conf.level = 1-significancia)
