
contraste_hipotesis1 <- function(media_x, media_y, p_x, p_y, sigma_x, sigma_y, n_x, n_y, CONOCIDAS=TRUE, IGUALES=TRUE, nula =0, significancia, tipo="superior", estadistico = "media"){

  if(estadistico == "media"){
  
# Dos medias, muestras independientes, varianzas poblacionales conocidas   
  
  if(CONOCIDAS = TRUE){
    
# Cola superior
    
    if(tipo=="superior") {

diferencia_medias <- media_x - media_y
dt <- sqrt((sigma_x/n_x)+(sigma_y/n_y))
alfa <- significancia
valor <- (diferencia_medias - nula) / dt
critico <- qnorm(p = alfa, lower.tail = FALSE)
pvalor <- pnorm(valor, lower.tail = FALSE)

    }
    
# Cola inferior

    if(tipo=="inferior") {
      
diferencia_medias <- media_x - media_y
dt <- sqrt((sigma_x/n_x)+(sigma_y/n_y))
alfa <- significancia
valor <- (diferencia_medias - nula) / dt
critico <- qnorm(p = alfa)
pvalor <- pnorm(valor)

    }
    
# Dos colas

    if(tipo=="bilateral") {
      
diferencia_medias <- media_x - media_y
dt <- sqrt((sigma_x/n_x)+(sigma_y/n_y))
alfa <- significancia/2
valor <- (diferencia_medias - nula) / dt
critico <- qnorm(p = alfa, lower.tail = FALSE)
pvalor <- pnorm(abs(valor), lower.tail = FALSE)*2

    }
    
}

# Dos medias, poblaciones independientes, varianzas desconocidas que se supone que son iguales --------

  if(CONOCIDAS = FALSE){
    
# Cola superior
  
  if(tipo=="superior") {

varianza_agrupada <- (((n_x-1)*varianza_x) + ((n_y-1)*varianza_y)) / (n_x + n_y -2)
dt <- sqrt((varianza_agrupada/n_x)+(varianza_agrupada/n_y))
gl <- n_x + n_y -2
diferencia_medias <- media_x - media_y
alfa <- significancia
valor <- (diferencia_medias - nula) / dt
critico <- qt(p = alfa, df = gl, lower.tail = FALSE)
pvalor <- pt(valor, df = gl, lower.tail = FALSE)

  }
  

# Cola inferior

  if(tipo=="inferior") {
    
varianza_agrupada <- (((n_x-1)*varianza_x) + ((n_y-1)*varianza_y)) / (n_x + n_y -2)
dt <- sqrt((varianza_agrupada/n_x)+(varianza_agrupada/n_y))
gl <- n_x + n_y -2
diferencia_medias <- media_x - media_y
alfa <- significancia
valor <- (diferencia_medias - nula) / dt
critico <- qt(p = alfa, df = gl)
pvalor <- pt(valor, df = gl)

  }
  
# Dos colas
  
  if(tipo=="bilateral") {

varianza_agrupada <- (((n_x-1)*varianza_x) + ((n_y-1)*varianza_y)) / (n_x + n_y -2)
dt <- sqrt((varianza_agrupada/n_x)+(varianza_agrupada/n_y))
gl <- n_x + n_y -2
diferencia_medias <- media_x - media_y
alfa <- significancia/2
valor <- (diferencia_medias - nula) / dt
critico <- qt(p = alfa, df = gl, lower.tail = FALSE)
pvalor <- pt(abs(valor), df = gl, lower.tail = FALSE)*2

  }
  
 }
    
}
  
  
# Contrastes de la diferencia entre dos proporciones poblacionales --------

  if(estadistico == "proporcion"){
  
# Cola superior
    
    if(tipo=="superior") {

diferencia_p <- p_x - p_y

p_0 <- ((n_x*p_x)+(n_y*p_y))/(n_x + n_y)

dt <- sqrt(((p_0*(1-p_0))/n_x) + ((p_0*(1-p_0))/n_y) )

alfa <- significancia
nula <- 0
valor <- (diferencia_p - nula) / dt
critico <- qnorm(p = alfa, lower.tail = FALSE)
pvalor <- pnorm(valor, lower.tail = FALSE)

    }
    
# Cola inferior
    
    if(tipo=="inferior") {

diferencia_p <- p_x - p_y

p_0 <- ((n_x*p_x)+(n_y*p_y))/(n_x + n_y)

dt <- sqrt(((p_0*(1-p_0))/n_x) + ((p_0*(1-p_0))/n_y) )

alfa <- significancia 
nula <- 0
valor <- (diferencia_p - nula) / dt
critico <- qnorm(p = alfa)
pvalor <- pnorm(valor)

    }
    
# Dos colas
    
    if(tipo=="bilateral") {

diferencia_p <- p_x - p_y

p_0 <- ((n_x*p_x)+(n_y*p_y))/(n_x + n_y)

dt <- sqrt(((p_0*(1-p_0))/n_x) + ((p_0*(1-p_0))/n_y) )

alfa <- significancia/2
nula <- 0
valor <- (diferencia_p - nula) / dt
critico <- qnorm(p = alfa, lower.tail = FALSE)
pvalor <- pnorm(abs(valor), lower.tail = FALSE)*2 

    }
    
}
