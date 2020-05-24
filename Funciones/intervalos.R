
intervalos <- function(media, proporcion, dt, n, confianza, estadistico = "media", CONOCIDA=TRUE){

  library(ggplot2)
  library(gridExtra)

  if(estadistico == "media"){
    
    if(CONOCIDA == TRUE){
# Intervalos de confianza para la media muestral cuando la varianza es conocida

confianza2 <- (1-confianza)/2

tabla <- qnorm(p = confianza2, lower.tail = FALSE)

ME <- tabla*(dt/sqrt(n))

LI <- media - ME
LS <- media + ME 

# Datos para el grafico

# Creamos dos vectores, uno con los datos de la variable (empleo un vector que va desde la media menos 5 veces la desviación típica muestral hasta la media mas 5 veces la desviación típica muestral)

lim_inf_graf <- media - 5*(dt / sqrt(n))
lim_sup_graf <- media + 5*(dt / sqrt(n))

x <- seq(from = lim_inf_graf, to = lim_sup_graf, by = 0.05 )

# Creamos la distribución normal estándar basados en los datos creados en el paso anterior
y <- dnorm((x - media) / (dt / sqrt(n)))

# Juntamos ambos vectores y hacemos una tabla (data frame). La libreria ggplot solo toma como argumentos arreglos tabulates de este tipo
df_dist <- data.frame(x=x, y=y)

    } else {

# Intervalos de confianza para la media muestral cuando la varianza es desconocida

gl <- n - 1

confianza2 <- (1-confianza)/2

tabla <- qt(p = confianza2, df = gl, lower.tail = FALSE)

ME <- tabla*(dt/sqrt(n))

LI <- media - ME
LS <- media + ME

# Datos para el grafico

# Creamos dos vectores, uno con los datos de la variable (empleo un vector que va desde la media menos 5 veces la desviación típica muestral hasta la media mas 5 veces la desviación típica muestral)

lim_inf_graf <- media - 5*(dt / sqrt(n))
lim_sup_graf <- media + 5*(dt / sqrt(n))

x <- seq(from = lim_inf_graf, to = lim_sup_graf, by = 0.01 )

# Creamos la distribución normal estándar basados en los datos creados en el paso anterior
y <- dt((x - media) / (dt / sqrt(n)), df = gl)

# Juntamos ambos vectores y hacemos una tabla (data frame). La libreria ggplot solo toma como argumentos arreglos tabulates de este tipo
df_dist <- data.frame(x=x, y=y)


    }
    
    tablon <- data.frame(
      "Concepto" = c("Nivel de confianza", "Margen de error", "Limite inferior", "Limite superior", "Estadístico"),
      "Datos"=c(confianza, ME, LI, LS, ifelse(estadistico == "proporcion", proporcion, media))
    )
    
    
    tablon$Datos <-  round(tablon$Datos, digits = 4)
    rownames(tablon) <- tablon[,1]
    tablon[,1] <- NULL
    colnames(tablon) <- ""
    
    
    p_int <-  ggplot(df_dist, aes(x = x, y = y)) +
      geom_line()  +
      geom_vline(xintercept = LI) +
      geom_vline(xintercept = LS) +
      geom_area(mapping = aes(ifelse(x>LI & x<LS,x,0)), fill = "turquoise2", alpha = 0.8) +
      xlim(lim_inf_graf,lim_sup_graf) +
      annotate("text", x = media, y = 0.15, label = paste0(as.character(confianza*100), "% de confianza"))+ 
      annotation_custom(tableGrob(tablon), xmin=lim_inf_graf, xmax=LI, ymin=0.2, ymax=0.4) +
      ggtitle("Intervalos de confianza de la media") +
      theme_classic()
    
    
  }
  

  if(estadistico == "proporcion"){
# Intervalos de confianza para la proporcion muestral

dt <- sqrt((proporcion*(1-proporcion))/n)

confianza2 <- (1-confianza)/2

tabla <- qnorm(p = confianza2, lower.tail = FALSE)

ME <- tabla*dt

LI <- proporcion - ME
LS <- proporcion + ME

# Datos para el grafico

# Creamos dos vectores, uno con los datos de la variable (empleo un vector que va desde la media menos 5 veces la desviación típica muestral hasta la media mas 5 veces la desviación típica muestral)

lim_inf_graf <- proporcion - 5*dt
lim_sup_graf <- proporcion + 5*dt

x <- seq(from = lim_inf_graf, to = lim_sup_graf, by = 0.001 )

# Creamos la distribución normal estándar basados en los datos creados en el paso anterior
y <- dnorm((x - proporcion) / dt)

# Juntamos ambos vectores y hacemos una tabla (data frame). La libreria ggplot solo toma como argumentos arreglos tabulates de este tipo
df_dist <- data.frame(x=x, y=y)

tablon <- data.frame(
  "Concepto" = c("Nivel de confianza", "Margen de error", "Limite inferior", "Limite superior", "Estadístico"),
  "Datos"=c(confianza, ME, LI, LS, ifelse(estadistico == "proporcion", proporcion, media))
)


tablon$Datos <-  round(tablon$Datos, digits = 4)
rownames(tablon) <- tablon[,1]
tablon[,1] <- NULL
colnames(tablon) <- ""

p_int <-  ggplot(df_dist, aes(x = x, y = y)) +
  geom_line()  +
  geom_vline(xintercept = LI) +
  geom_vline(xintercept = LS) +
  geom_area(mapping = aes(ifelse(x>LI & x<LS,x,0)), fill = "turquoise2", alpha = 0.8) +
  xlim(lim_inf_graf,lim_sup_graf) +
  annotate("text", x = proporcion, y = 0.15, label = paste0(as.character(confianza*100), "% de confianza")) + 
  annotation_custom(tableGrob(tablon), xmin=lim_inf_graf, xmax=LI, ymin=0.2, ymax=0.4) +
  ggtitle("Intervalos de confianza de una proporción") +
  theme_classic()


  }
  
  return(list(Resumen =tablon,p_int))

}

# Ejemplo 7.4
intervalos(media = 18.68, dt = 1.69526, n = 24, CONOCIDA = FALSE, confianza = .9, estadistico = "media")
# Ejemplo 7.3
intervalos(media = 75, dt = 20,n = 64, CONOCIDA = TRUE, confianza = .95, estadistico = "media")
# Ejemplo 7.6
intervalos(proporcion = 0.759, n = 344, confianza = .9, estadistico = "proporcion")


