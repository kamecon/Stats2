
contraste_hipotesis2 <- function(media_x, media_y, p_x, p_y, sigma_x, sigma_y, n_x, n_y, CONOCIDAS=TRUE, IGUALES=TRUE, nula =0, significancia, tipo="superior", estadistico = "media"){

  library(ggplot2)
  library(gridExtra)
  
  if(estadistico == "media"){
  
# Dos medias, muestras independientes, varianzas poblacionales conocidas   
  
  if(CONOCIDAS == TRUE){
    
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

  if(CONOCIDAS == FALSE){
    
# Cola superior
  
  if(tipo=="superior") {

varianza_agrupada <- (((n_x-1)*sigma_x) + ((n_y-1)*sigma_y)) / (n_x + n_y -2)
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
    
varianza_agrupada <- (((n_x-1)*sigma_x) + ((n_y-1)*sigma_y)) / (n_x + n_y -2)
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

varianza_agrupada <- (((n_x-1)*sigma_x) + ((n_y-1)*sigma_y)) / (n_x + n_y -2)
dt <- sqrt((varianza_agrupada/n_x)+(varianza_agrupada/n_y))
gl <- n_x + n_y -2
diferencia_medias <- media_x - media_y
alfa <- significancia/2
valor <- (diferencia_medias - nula) / dt
critico <- qt(p = alfa, df = gl, lower.tail = FALSE)
pvalor <- pt(abs(valor), df = gl, lower.tail = FALSE)*2

  }
  
  }
    
    if(tipo=="bilateral") {
      
      tabla <- data.frame(
        "Concepto" = c("Valor", "Valor crítico inferior", "Valor crítico superior", "p-valor", "significancia"),
        "Datos"=c(valor, critico*(-1), critico, pvalor, significancia)
      )
      
      tabla$Datos <-  round(tabla$Datos, digits = 4)
      rownames(tabla) <- tabla[,1]
      tabla[,1] <- NULL
      colnames(tabla) <- ""
      
      p1 <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
        stat_function(fun = dnorm,  size = 1.5) + geom_vline(xintercept = valor, color="red", linetype="dashed", size=1.5) + 
        stat_function(fun = dnorm, geom="area", fill="turquoise2", alpha=0.4, xlim = c(-4,critico*(-1))) +
        stat_function(fun = dnorm, geom="area", fill="turquoise2", alpha=0.4, xlim = c(critico,4)) + 
        annotation_custom(tableGrob(tabla), xmin=-4, xmax=-2, ymin=0.2, ymax=0.4) +
        ggtitle("Contraste de hipótesis de diferencia de medias", subtitle = paste0("Cola ", tipo)) +
        theme_classic()
      
      return(list(Resumen =tabla,p1)) } else {
        
        tabla <- data.frame(
          "Concepto" = c("Valor", "Valor crítico", "p-valor", "significancia"),
          "Datos"=c(valor, critico, pvalor, significancia)
        )
        
        tabla$Datos <-  round(tabla$Datos, digits = 4)
        rownames(tabla) <- tabla[,1]
        tabla[,1] <- NULL
        colnames(tabla) <- ""
      }
        
        if(tipo == "inferior"){
          
          p1 <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
            stat_function(fun = dnorm,  size = 1.5) + geom_vline(xintercept = valor, color="red", linetype="dashed", size=1.5) + 
            stat_function(fun = dnorm, geom="area", fill="turquoise2", alpha=0.4, xlim = c(-4,critico)) + 
            annotation_custom(tableGrob(tabla), xmin=-4, xmax=-2, ymin=0.2, ymax=0.4) +
            ggtitle("Contraste de hipótesis de diferencia de medias", subtitle = paste0("Cola ", tipo)) +
            theme_classic() } else {
              
              p1 <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
                stat_function(fun = dnorm, size = 1.5) + geom_vline(xintercept = valor, color="red", linetype="dashed", size=1.5) + 
                stat_function(fun = dnorm, geom="area", fill="turquoise2", alpha=0.4, xlim = c(critico,4)) + 
                annotation_custom(tableGrob(tabla), xmin=-4, xmax=-2, ymin=0.2, ymax=0.4) +
                ggtitle("Contraste de hipótesis de diferencia de medias", subtitle = paste0("Cola ", tipo)) +
                theme_classic()
            }
        
        return(list(Resumen =tabla,p1))
        
    
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
  
  if(tipo=="bilateral") {
    
    tabla <- data.frame(
      "Concepto" = c("Valor", "Límite inferior", "Límite superior", "p-valor", "significancia"),
      "Datos"=c(valor, critico*(-1), critico, pvalor, significancia)
    )
    
    tabla$Datos <-  round(tabla$Datos, digits = 4)
    rownames(tabla) <- tabla[,1]
    tabla[,1] <- NULL
    colnames(tabla) <- ""
    
    p1 <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
      stat_function(fun = dnorm,  size = 1.5) + geom_vline(xintercept = valor, color="red", linetype="dashed", size=1.5) + 
      stat_function(fun = dnorm, geom="area", fill="turquoise2", alpha=0.4, xlim = c(-4,critico*(-1))) +
      stat_function(fun = dnorm, geom="area", fill="turquoise2", alpha=0.4, xlim = c(critico,4)) + 
      annotation_custom(tableGrob(tabla), xmin=-4, xmax=-2, ymin=0.2, ymax=0.4) +
      ggtitle("Contraste de hipótesis de la proporción", subtitle = paste0("Cola ", tipo)) +
      theme_classic()
    
    return(list(Resumen =tabla,p1)) } else {
      
      tabla <- data.frame(
        "Concepto" = c("Valor", "Valor crítico", "p-valor", "significancia"),
        "Datos"=c(valor, critico, pvalor, significancia)
      )
      
      tabla$Datos <-  round(tabla$Datos, digits = 4)
      rownames(tabla) <- tabla[,1]
      tabla[,1] <- NULL
      colnames(tabla) <- ""
      
      if(tipo == "inferior"){
        
        p1 <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
          stat_function(fun = dnorm,  size = 1.5) + geom_vline(xintercept = valor, color="red", linetype="dashed", size=1.5) + 
          stat_function(fun = dnorm, geom="area", fill="turquoise2", alpha=0.4, xlim = c(-4,critico)) + 
          annotation_custom(tableGrob(tabla), xmin=-4, xmax=-2, ymin=0.2, ymax=0.4) +
          ggtitle("Contraste de hipótesis de la proporción", subtitle = paste0("Cola ", tipo)) +
          theme_classic() } else {
            
            p1 <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
              stat_function(fun = dnorm, size = 1.5) + geom_vline(xintercept = valor, color="red", linetype="dashed", size=1.5) + 
              stat_function(fun = dnorm, geom="area", fill="turquoise2", alpha=0.4, xlim = c(critico,4)) + 
              annotation_custom(tableGrob(tabla), xmin=-4, xmax=-2, ymin=0.2, ymax=0.4) +
              ggtitle("Contraste de hipótesis de la proporción", subtitle = paste0("Cola ", tipo)) +
              theme_classic()
          }
      
      return(list(Resumen =tabla,p1))
      
  
    }
  
}



contraste_hipotesis2(media_x = 1078, media_y = 908.2, sigma_x = 633^2, sigma_y = 469.8^2, CONOCIDAS = FALSE, IGUALES = TRUE, nula = 0, significancia = 0.05, estadistico = "media", tipo = "superior", n_x = 25, n_y = 25)

contraste_hipotesis2(p_x = 52/203, p_y = 56/270, n_x = 203, n_y = 270, nula = 0, estadistico = "proporcion", tipo = "bilateral", significancia = 0.1)

#10.8
contraste_hipotesis2(media_x = 85.8, media_y = 71.5, n_x = 151, n_y = 108, sigma_x = 19.13^2, sigma_y = 12.2^2, CONOCIDAS = TRUE, IGUALES = TRUE, nula = 0, significancia = 0.05, estadistico = "media", tipo = "superior")

#10.9
contraste_hipotesis2(media_x = 1.91, media_y = 0.21, n_x = 125, n_y = 86, sigma_x = 1.32^2, sigma_y = 0.53^2, CONOCIDAS = TRUE, IGUALES = TRUE, nula = 0, significancia = 0.05, estadistico = "media", tipo = "superior")

#10.11
contraste_hipotesis2(media_x = 36.21, media_y = 47.56, n_x = 36, n_y = 36, sigma_x = 22.93^2, sigma_y = 27.56^2, CONOCIDAS = FALSE, IGUALES = TRUE, nula = 0, significancia = 0.05, estadistico = "media", tipo = "bilateral")

#10.12
contraste_hipotesis2(media_x = 3.97, media_y = 2.86, n_x = 70, n_y = 51, sigma_x = 6.14^2, sigma_y = 4.29^2, CONOCIDAS = FALSE, IGUALES = TRUE, nula = 0, significancia = 0.1, estadistico = "media", tipo = "bilateral")

#10.13
contraste_hipotesis2(media_x = 9254, media_y = 8167, n_x = 10, n_y = 10, sigma_x = 2107^2, sigma_y = 1681^2, CONOCIDAS = FALSE, IGUALES = TRUE, nula = 0, significancia = 0.1, estadistico = "media", tipo = "superior")

#10.15
contraste_hipotesis2(p_x = .6, p_y = .66, n_x = 900, n_y = 900, nula = 0, estadistico = "proporcion", tipo = "inferior", significancia = 0.05)

#10.16
contraste_hipotesis2(p_x = .384, p_y = .52, n_x = 1556, n_y = 1108, nula = 0, estadistico = "proporcion", tipo = "inferior", significancia = 0.05)

#10.17
contraste_hipotesis2(p_x = 92/368, p_y = 37/116, n_x = 368, n_y = 116, nula = 0, estadistico = "proporcion", tipo = "bilateral", significancia = 0.05)
