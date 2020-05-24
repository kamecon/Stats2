
intervalos2 <- function(media_x, media_y, p_x, p_y, varianza_x, varianza_y, n_x, n_y, confianza, CONOCIDAS=TRUE, IGUALES=TRUE, estadistico = "media") {
  
  # Calcula intervalos de confianza para la diferencia de medias y proporciones
  # Autor: Kamal Romero (karomero@ucm.es)
  #
  # Dependiencias: Necesita las librerias ggplot y gridExtra
  # Args:
  #   media_x: media muestra x
  #   media_y: media muestra y
  #   p_x: proporcion muestra x (debe ser un número estre cero y uno)
  #   p_y: proporcion muestra y (debe ser un número estre cero y uno)
  #   varianza_x: varianza muestra x
  #   varianza_y: varianza muestra y
  #   n_x: tamaño de la muestra x
  #   n_y: tamaño de la muestra y
  #   estadistico: el estadístico muestral, de ser "media" o "proporcion" (default = "media")
  #   confianza: nivel de confianza del intervalo (debe ser un número estre cero y uno)
  #   CONOCIDAS: TRUE si las varianzas son conocidas y FALSE en caso contrario (default TRUE)
  #   IGUALES: TRUE si las varianzas son desconocidas e iguales y FALSE en caso contrario (default TRUE)
  #
  # Returns:
  #   Una tabla con los límites del intervalo
  #   Un gráfico con la distribución, los límites y la tabla
  #


  if(estadistico == "media"){
    
    # Dos medias, muestras independientes, varianzas poblacionales conocidas   
    
    if(CONOCIDAS == TRUE){
      
  
# Dos medias, muestras independientes, varianzas poblacionales conocidas 

diferencia <- media_x - media_y
confianza2 <- (1-confianza)/2
tabla <- qnorm(p = confianza2, lower.tail = FALSE)

ME <- tabla*sqrt((varianza_x/n_x)+(varianza_y/n_y))
LI <- diferencia - ME
LS <- diferencia + ME

tablon <- data.frame(
  "Concepto" = c("Nivel de confianza", "Limite inferior", "Limite superior", "Diferencia"),
  "Datos"=c(confianza, LI, LS, diferencia)
)


tablon$Datos <-  round(tablon$Datos, digits = 4)
rownames(tablon) <- tablon[,1]
tablon[,1] <- NULL
colnames(tablon) <- ""

}

    if(CONOCIDAS == FALSE){
      
      if(IGUALES == TRUE) {
        
# Dos medias, poblaciones independientes, varianzas desconocidas que se supone que son iguales 

varianza_agrupada <- (((n_x-1)*varianza_x) + ((n_y-1)*varianza_y)) / (n_x + n_y -2)

# Los grados de libertad 

gl <- n_x + n_y -2

diferencia <- media_x - media_y
confianza2 <- (1-confianza)/2
tabla <- qt(p = confianza2, df = gl, lower.tail = FALSE)

ME <- tabla*sqrt((varianza_agrupada/n_x)+(varianza_agrupada/n_y))
LI <- diferencia - ME
LS <- diferencia + ME

} else {

# Dos medias, poblaciones independientes, varianzas desconocidas que se supone que son distintas 

diferencia <- media_x - media_y

# Calculamos los grados de libertad

gl <- (((varianza_x/n_x) + (varianza_y/n_y))^2) / (((1/(n_x - 1))*(varianza_x/n_x)^2) + ((1/(n_y - 1))*(varianza_y/n_y)^2))
gl_redondo <- round(gl)

confianza2 <- (1-confianza)/2
tabla <- qt(p = confianza2, df = gl_redondo, lower.tail = FALSE)

ME <- tabla*sqrt((varianza_x/n_x)+(varianza_y/n_y))
LI <- diferencia - ME
LS <- diferencia + ME

}
      tablon <- data.frame(
        "Concepto" = c("Nivel de confianza", "Grados de libertad", "Limite inferior", "Limite superior", "Diferencia"),
        "Datos"=c(confianza, gl, LI, LS, diferencia)
      )
      
      
      tablon$Datos <-  round(tablon$Datos, digits = 4)
      rownames(tablon) <- tablon[,1]
      tablon[,1] <- NULL
      colnames(tablon) <- ""
      
      }
    
    
  }


if(estadistico == "proporcion"){
  
# Contrastes de la diferencia entre dos proporciones poblacionales 
  
diferencia_p <- p_x - p_y

dt <- sqrt(((p_x*(1-p_x))/n_x) + ((p_y*(1-p_y))/n_y) )
confianza2 <- (1 - confianza)/2
tabla <- qnorm(p = confianza2, lower.tail = FALSE)

ME <- tabla*dt
LI <- diferencia_p - ME
LS <- diferencia_p + ME

tablon <- data.frame(
  "Concepto" = c("Nivel de confianza", "Limite inferior", "Limite superior", "Diferencia"),
  "Datos"=c(confianza, LI, LS, diferencia_p)
)


tablon$Datos <-  round(tablon$Datos, digits = 4)
rownames(tablon) <- tablon[,1]
tablon[,1] <- NULL
colnames(tablon) <- ""


}

  return(tablon)
}

#Ejemplo 8.3
intervalos2(media_x = 3.08, media_y = 2.88, varianza_x = 0.42^2, varianza_y = 0.64^2, n_x = 120, n_y = 90, confianza = .95, estadistico = "media", CONOCIDAS = TRUE)
#Ejemplo 8.4
intervalos2(media_x = 133.3, media_y = 94, n_x = 10, n_y = 8, varianza_x = 218.011, varianza_y = 129.4256, confianza = .95, estadistico = "media",CONOCIDAS = FALSE, IGUALES = TRUE)
#Ejemplo 8.5
intervalos2(media_x = 290, media_y = 250, n_x = 16, n_y = 11, varianza_x = 15^2, varianza_y = 50^2, confianza = .95,  estadistico = "media", CONOCIDAS = FALSE, IGUALES = FALSE)
#Ejemplo 8.6
intervalos2(p_x = .892, p_y = .518, n_x = 120, n_y = 141, confianza = .95,  estadistico = "proporcion")


