
contraste_hipotesis1 <- function(media, media_muestral, proporcion_poblacional, proporcion_muestral, dtd=NULL, dtc=NULL, significancia, n, tipo="superior", estadistico = "media"){

  # Realiza contrastes de hipótesis para la media y la proporción
  # Autor: Kamal Romero (karomero@ucm.es)
  #
  # Args:
  #   media: media poblacional
  #   media_muestral: media muestral
  #   proporcion_poblacional: proporcion poblacional (debe ser un número estre cero y uno)
  #   proporcion_muestral: proporcion muestral (debe ser un número estre cero y uno)
  #   dtc: desviación típica si esta es conocida
  #   dtd: desviación típica si esta es desconocida
  #   n: tamaño de la muestra
  #   significancia: nivel de significancia del contraste (debe ser un número estre cero y uno)
  #   estadistico: el estadístico de contraste, de ser "media" o "proporcion" (default = "media")
  #   tipo: si el tipo de contraste es de cola superior, cola inferior o bilateral (default = "superior")
  #
  # Returns:
  #   Una tabla con el estadístico de contraste (Valor), el valor crítico (Valor crítico), 
  #   la media o proporción muestral, el valor crítco medido en la escala original (Límite)
  #   el p-valor y el nivel de significancia
  #
  
  
# Contrastes de la media de una distribución normal: varianza poblacional conocida 

  library(ggplot2)
  library(gridExtra)
  
  if(estadistico == "media"){
  
  if(!is.null(dtc)){
    
    z_estandar <-  (media_muestral - media)/(dtc / sqrt(n))
  
# Contraste de cola superior

  if(tipo=="superior") {

confianza2 <- significancia
tabla <- qnorm(p = confianza2, lower.tail = FALSE)
valor <- z_estandar
critico <- media + tabla*(dtc/sqrt(n))
pvalor <- pnorm(valor, lower.tail = FALSE)

  }
  
# Contraste de cola inferior
  
  if(tipo=="inferior") {

confianza2 <- significancia
tabla <- qnorm(p = confianza2, lower.tail = FALSE)*(-1)
valor <- z_estandar
critico <- media + tabla*(dtc/sqrt(n))
pvalor <- pnorm(valor)

  }
  
# Contraste de dos colas
  
  if(tipo=="bilateral") {
  
confianza2 <- significancia/2
tabla <- qnorm(p = confianza2, lower.tail = FALSE)
valor <- z_estandar
critico1 <- media + tabla*(dtc/sqrt(n))
critico2 <- media - tabla*(dtc/sqrt(n))
pvalor <- pnorm(abs(valor), lower.tail = FALSE)*2

  }
  
  }
  
# Contrastes de la media de una distribución normal: varianza poblacional desconocida

  if(!is.null(dtd)){
    
    z_estandar <-  (media_muestral - media)/(dtd / sqrt(n))
  
# Contraste de cola superior
  if(tipo=="superior") {

confianza2 <- significancia
tabla <- qt(p = confianza2, df = n-1, lower.tail = FALSE)
valor <- z_estandar
critico <- media + tabla*(dtd/sqrt(n))
pvalor <- pt(valor,df = n-1, lower.tail = FALSE)

  }
  
# Contraste de cola inferior
  if(tipo=="inferior") {

confianza2 <- signiicancia
tabla <- qt(p = confianza2, df = n-1, lower.tail = FALSE)*(-1)
valor <- z_estandar
critico <- media + tabla*(dtd/sqrt(n))
pvalor <- pt(valor, df = n-1)

  }
  
# Contraste de dos colas
  if(tipo=="bilateral") {

confianza2 <- significancia/2
tabla <- qt(p = confianza2, df = n-1, lower.tail = FALSE)
valor <- z_estandar
critico1 <- media + tabla*(dtd/sqrt(n))
critico2 <- media - tabla*(dtd/sqrt(n))
pvalor <- pt(abs(valor), df = n-1, lower.tail = FALSE)*2

  }
  
  }
  
  if(tipo=="bilateral") {
    
    tablon <- data.frame(
      "Concepto" = c("Valor", "Valor crítico superior", "Valor crítico inferior", "Media muestral", "Límite inferior", "Límite superior",  "p-valor", "significancia"),
      "Datos"=c(valor, tabla*(-1), tabla, media_muestral, critico1, critico2, pvalor, (significancia/2))
    )
    
    tablon$Datos <-  round(tablon$Datos, digits = 4)
    rownames(tablon) <- tablon[,1]
    tablon[,1] <- NULL
    colnames(tablon) <- ""
    
    return(tablon)
    
  } else {
  
  tablon <- data.frame(
    "Concepto" = c("Estadistico contraste", "Valor crítico", "Media muestral", "Limite", "p-valor", "significancia"),
    "Datos"=c(valor, tabla, media_muestral, critico, pvalor, significancia)
  )
  
  tablon$Datos <-  round(tablon$Datos, digits = 4)
  rownames(tablon) <- tablon[,1]
  tablon[,1] <- NULL
  colnames(tablon) <- ""
  
  if(tipo == "inferior"){
    
    p1 <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
      stat_function(fun = dnorm,  size = 1.5) + geom_vline(xintercept = valor, color="red", linetype="dashed", size=1.5) + 
      stat_function(fun = dnorm, geom="area", fill="turquoise2", alpha=0.4, xlim = c(-4,tabla)) + 
      annotation_custom(tableGrob(tablon), xmin=-4, xmax=-2, ymin=0.2, ymax=0.4) +
      ggtitle("Contraste de hipótesis de la media", subtitle = paste0("Cola ", tipo)) +
      theme_classic() } else {
        
    p1 <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
          stat_function(fun = dnorm, size = 1.5) + geom_vline(xintercept = valor, color="red", linetype="dashed", size=1.5) + 
          stat_function(fun = dnorm, geom="area", fill="turquoise2", alpha=0.4, xlim = c(tabla,4)) + 
          annotation_custom(tableGrob(tablon), xmin=-4, xmax=-2, ymin=0.2, ymax=0.4) +
          ggtitle("Contraste de hipótesis de la media", subtitle = paste0("Cola ", tipo)) +
          theme_classic()
      }
  
  return(list(Resumen =tablon,p1))
  
  }
}

  if(estadistico == "proporcion"){
    
# Contrastes de la proporción poblacional 
    
    if (proporcion_muestral > 1 | proporcion_muestral < 0) {
      stop(" La proporcion debe ser un numero entre cero y uno", call. = FALSE)
    }
    
    if (proporcion_poblacional > 1 | proporcion_poblacional < 0) {
      stop(" La proporcion debe ser un numero entre cero y uno", call. = FALSE)
    }
    
    # Contraste de cola superior
    
    if(tipo=="superior") {
    
    dt <- sqrt((proporcion_poblacional*(1-proporcion_poblacional))/n)
    confianza2 <- significancia
    tabla_z <- qnorm(p = confianza2, lower.tail = FALSE)
    valor <- (proporcion_muestral - proporcion_poblacional)/dt
    critico <- proporcion_poblacional + tabla_z*dt
    pvalor <- pnorm(valor, lower.tail = FALSE)
    
    }
    
    
    # Contraste de cola inferior
    
    if(tipo=="inferior") {
      
      dt <- sqrt((proporcion_poblacional*(1-proporcion_poblacional))/n)
      confianza2 <- significancia
      tabla_z <- qnorm(p = confianza2, lower.tail = FALSE)*(-1)
      valor <- (proporcion_muestral - proporcion_poblacional)/dt
      critico <- proporcion_poblacional - tabla_z*dt
      pvalor <- pnorm(valor)
      
    }
    
    
    # Contraste de dos colas
    
    if(tipo=="bilateral") {
      
      dt <- sqrt((proporcion_poblacional*(1-proporcion_poblacional))/n)
      confianza2 <- significancia/2
      tabla_z <- qnorm(p = confianza2, lower.tail = FALSE)
      valor <- (proporcion_muestral - proporcion_poblacional)/dt
      critico1 <- proporcion_poblacional - tabla_z*dt
      critico2 <- proporcion_poblacional + tabla_z*dt
      pvalor <- pnorm(abs(valor), lower.tail = FALSE)*2
      
    }
    
    if(tipo=="bilateral") {
      
      tabla <- data.frame(
        "Concepto" = c("Valor", "Valor crítico inferior", "Valor crítico superior", "Proporción muestral", "Límite inferior", "Límite superior", "p-valor", "significancia"),
        "Datos"=c(valor, tabla_z*(-1), tabla_z, proporcion_muestral, critico1, critico2,  pvalor, significancia)
      )
      
      tabla$Datos <-  round(tabla$Datos, digits = 4)
      rownames(tabla) <- tabla[,1]
      tabla[,1] <- NULL
      colnames(tabla) <- ""
      
      p1 <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
        stat_function(fun = dnorm,  size = 1.5) + geom_vline(xintercept = valor, color="red", linetype="dashed", size=1.5) + 
        stat_function(fun = dnorm, geom="area", fill="turquoise2", alpha=0.4, xlim = c(-4,tabla_z*(-1))) +
        stat_function(fun = dnorm, geom="area", fill="turquoise2", alpha=0.4, xlim = c(tabla_z,4)) + 
        annotation_custom(tableGrob(tabla), xmin=-4, xmax=-2, ymin=0.2, ymax=0.4) +
        ggtitle("Contraste de hipótesis de la proporción", subtitle = paste0("Cola ", tipo)) +
        theme_classic()
      
      return(list(Resumen =tabla,p1)) } else {
        
        tabla <- data.frame(
          "Concepto" = c("Valor", "Valor crítico", "Proporción muestral", "Límite",  "p-valor", "significancia"),
          "Datos"=c(valor, tabla_z, proporcion_muestral, critico, pvalor, significancia)
        )
        
        tabla$Datos <-  round(tabla$Datos, digits = 4)
        rownames(tabla) <- tabla[,1]
        tabla[,1] <- NULL
        colnames(tabla) <- ""
        
        if(tipo == "inferior"){
        
        p1 <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
          stat_function(fun = dnorm,  size = 1.5) + geom_vline(xintercept = valor, color="red", linetype="dashed", size=1.5) + 
          stat_function(fun = dnorm, geom="area", fill="turquoise2", alpha=0.4, xlim = c(-4,tabla_z)) + 
          annotation_custom(tableGrob(tabla), xmin=-4, xmax=-2, ymin=0.2, ymax=0.4) +
          ggtitle("Contraste de hipótesis de la proporción", subtitle = paste0("Cola ", tipo)) +
          theme_classic() } else {
          
        p1 <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
              stat_function(fun = dnorm, size = 1.5) + geom_vline(xintercept = valor, color="red", linetype="dashed", size=1.5) + 
              stat_function(fun = dnorm, geom="area", fill="turquoise2", alpha=0.4, xlim = c(tabla_z,4)) + 
              annotation_custom(tableGrob(tabla), xmin=-4, xmax=-2, ymin=0.2, ymax=0.4) +
              ggtitle("Contraste de hipótesis de la proporción", subtitle = paste0("Cola ", tipo)) +
              theme_classic()
          }
        
        return(list(Resumen =tabla,p1))
        
      }
    
  }  
    
}



# Ejemplos

contraste_hipotesis1(media = 2400, media_muestral = 3593, dtd = 4919, significancia = 0.05, n = 134,estadistico = "media", tipo = "superior")
contraste_hipotesis1(media = 5, media_muestral = 4.962, dtc = 0.1, significancia = 0.05, n = 16,estadistico = "media", tipo = "inferior")
contraste_hipotesis1(media = 80, media_muestral = 83, dtc = 8, significancia = 0.05, n = 25,estadistico = "media", tipo = "superior")
contraste_hipotesis1(proporcion_poblacional = 0.5, proporcion_muestral = 28/50, significancia = 0.05, estadistico = "proporcion", tipo = "superior", n = 50)
contraste_hipotesis1(proporcion_poblacional = 0.5, proporcion_muestral = 378/802, n = 802,significancia = 0.07, estadistico = "proporcion", tipo = "inferior")
contraste_hipotesis1(proporcion_poblacional = 0.5, proporcion_muestral = 104/199, n = 199,significancia = 0.1, estadistico = "proporcion", tipo = "bilateral")
