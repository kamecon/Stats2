# 11.4 ----

# Varianzas conocidas

n_x <- 151
n_y <- 108

media_x <- 85.8 
media_y <- 71.5

sigma_x <- 19.13^2
sigma_y <- 12.2^2

diferencia_medias <- media_x - media_y

dt <- sqrt((sigma_x/n_x)+(sigma_y/n_y))

alfa <- 0.05
nula <- 0
valor <- (diferencia_medias - nula) / dt
critico <- qnorm(p = alfa, lower.tail = FALSE)
pvalor <- pnorm(valor, lower.tail = FALSE)



# 11.5 ----

n_x <- 125
n_y <- 86

media_x <- 1.91 
media_y <- 0.21

sigma_x <- 1.32^2
sigma_y <- 0.53^2

diferencia_medias <- media_x - media_y

dt <- sqrt((sigma_x/n_x)+(sigma_y/n_y))

alfa <- 0.05
nula <- 0
valor <- (diferencia_medias - nula) / dt
critico <- qnorm(p = alfa, lower.tail = FALSE)
pvalor <- pnorm(valor, lower.tail = FALSE)



# 11.7 ----

n_x <- 36
n_y <- 36

media_x <- 36.21
media_y <- 47.56

varianza_x <- 22.93^2
varianza_y <- 27.56^2


varianza_agrupada <- (((n_x-1)*varianza_x) + ((n_y-1)*varianza_y)) / (n_x + n_y -2)
dt <- sqrt((varianza_agrupada/n_x)+(varianza_agrupada/n_y))

# Los grados de libertad 

gl <- n_x + n_y -2

# Calculamos la diferencia de medias

diferencia_medias <- media_x - media_y

alfa <- 0.05/2 #Dividimos entre 2 porque es de dos colas
nula <- 0
valor <- (diferencia_medias - nula) / dt
critico <- qt(p = alfa, df = gl, lower.tail = FALSE)
pvalor <- pt(valor, df = gl)*2 #Se multiplica entre 2 porque es una alternativa bilateral (dos colas)


# 11.9 ----

n_x <- 10
n_y <- 10

media_x <- 9254
media_y <- 8167

varianza_x <- 2107^2
varianza_y <- 1681^2


varianza_agrupada <- (((n_x-1)*varianza_x) + ((n_y-1)*varianza_y)) / (n_x + n_y -2)
dt <- sqrt((varianza_agrupada/n_x)+(varianza_agrupada/n_y))

# Los grados de libertad 

gl <- n_x + n_y -2

# Calculamos la diferencia de medias

diferencia_medias <- media_x - media_y

alfa <- 0.1
nula <- 0
valor <- (diferencia_medias - nula) / dt
critico <- qt(p = alfa, df = gl, lower.tail = FALSE)
pvalor <- pt(valor, df = gl, lower.tail = FALSE)


# 11.11 ----

Student_Pair <- read.csv("/cloud/project/Clases/Student Pair.csv", sep=";", stringsAsFactors=FALSE)

t.test(x = Student_Pair$COURSE, y = Student_Pair$NO.COURSE, mu = 0, alternative = "greater", var.equal = TRUE, conf.level = 0.95)

t.test(x = Student_Pair$COURSE, y = Student_Pair$NO.COURSE, mu = 0, alternative = "greater", var.equal = FALSE, conf.level = 0.95)


# 11.13 ----

n_a <- 900
n_b <- 900

p_a <- 0.6
p_b <- 0.66

# Calculamos la diferencia de proporciones

diferencia_p <- p_a - p_b

# Calculamos la proporción desconocida

p_0 <- ((n_a*p_a)+(n_b*p_b))/(n_a + n_b)

# Calculamos la desviación típica

dt <- sqrt(((p_0*(1-p_0))/n_a) + ((p_0*(1-p_0))/n_b) )

alfa <- 0.10
nula <- 0
valor <- (diferencia_p - nula) / dt
critico <- qnorm(p = alfa) #Como es un contraste de cola inferior quitamos el `lower.tail = FALSE`
pvalor <- pnorm(valor) #Como es un contraste de cola inferior quitamos el `lower.tail = FALSE`

# 11.15 ----

n_a <- 368
n_b <- 116

p_a <- 92/n_a
p_b <- 37/n_b

# Calculamos la diferencia de proporciones

diferencia_p <- p_a - p_b

# Calculamos la proporción desconocida

p_0 <- ((n_a*p_a)+(n_b*p_b))/(n_a + n_b)

# Calculamos la desviación típica

dt <- sqrt(((p_0*(1-p_0))/n_a) + ((p_0*(1-p_0))/n_b) )

alfa <- 0.05/2 #Se divide entre 2 porque es una alternativa bilateral (dos colas)
nula <- 0
valor <- (diferencia_p - nula) / dt
critico <- qnorm(p = alfa, lower.tail = FALSE) 
pvalor <- pnorm(valor, lower.tail = FALSE)*2 #Se multiplica entre 2 porque es una alternativa bilateral (dos colas)

# 11.19 ----

n_a <- 1200
n_b <- 1000

p_a <- 480/n_a
p_b <- 790/n_b

# Calculamos la diferencia de proporciones

diferencia_p <- p_a - p_b

# Calculamos la proporción desconocida

p_0 <- ((n_a*p_a)+(n_b*p_b))/(n_a + n_b)

# Calculamos la desviación típica

dt <- sqrt(((p_0*(1-p_0))/n_a) + ((p_0*(1-p_0))/n_b) )

alfa <- 0.01
nula <- 0
valor <- (diferencia_p - nula) / dt
critico <- qnorm(p = alfa) #Como es un contraste de cola inferior quitamos el `lower.tail = FALSE`
pvalor <- pnorm(valor) #Como es un contraste de cola inferior quitamos el `lower.tail = FALSE`


# 11.37 ----

n_x <- 70
n_y <- 106

media_x <- 4.4
media_y <- 5.3 

varianza_x <- 1.3^2
varianza_y <- 1.4^2


varianza_agrupada <- (((n_x-1)*varianza_x) + ((n_y-1)*varianza_y)) / (n_x + n_y -2)
dt <- sqrt((varianza_agrupada/n_x)+(varianza_agrupada/n_y))

# Los grados de libertad 

gl <- n_x + n_y -2

# Calculamos la diferencia de medias

diferencia_medias <- media_x - media_y

alfa <- 0.05
nula <- 0
valor <- (diferencia_medias - nula) / dt
critico <- qt(p = alfa, df = gl) #Como es un contraste de cola inferior quitamos el `lower.tail = FALSE`
pvalor <- pt(valor, df = gl) #Como es un contraste de cola inferior quitamos el `lower.tail = FALSE`

# 11.39 ----

n_x <- 4
n_y <- 8

media_x <- 78
media_y <- 114.7 

varianza_x <- 24.4^2
varianza_y <- 14.6^2


varianza_agrupada <- (((n_x-1)*varianza_x) + ((n_y-1)*varianza_y)) / (n_x + n_y -2)
dt <- sqrt((varianza_agrupada/n_x)+(varianza_agrupada/n_y))

# Los grados de libertad 

gl <- n_x + n_y -2

# Calculamos la diferencia de medias

diferencia_medias <- media_x - media_y

alfa <- 0.01
nula <- 0
valor <- (diferencia_medias - nula) / dt
critico <- qt(p = alfa, df = gl) #Como es un contraste de cola inferior quitamos el `lower.tail = FALSE`
pvalor <- pt(valor, df = gl) #Como es un contraste de cola inferior quitamos el `lower.tail = FALSE`

# 11.43 ----

n_x <- 83
n_y <- 54

media_x <- 6.543
media_y <- 6.733 

varianza_x <- 0.649^2
varianza_y <- 0.425^2


varianza_agrupada <- (((n_x-1)*varianza_x) + ((n_y-1)*varianza_y)) / (n_x + n_y -2)
dt <- sqrt((varianza_agrupada/n_x)+(varianza_agrupada/n_y))

# Los grados de libertad 

gl <- n_x + n_y -2

# Calculamos la diferencia de medias

diferencia_medias <- media_x - media_y

alfa <- 0.1/2 #Se divide entre 2 porque es una alternativa bilateral (dos colas)
nula <- 0
valor <- (diferencia_medias - nula) / dt
critico <- qt(p = alfa, df = gl) 
pvalor <- pt(valor, df = gl)*2 #Se multiplica entre 2 porque es una alternativa bilateral (dos colas)

# 11.45 ----

n_x <- 23
n_y <- 23

media_x <- 0.058
media_y <- 0.146 

varianza_x <- 0.055^2
varianza_y <- 0.058^2


varianza_agrupada <- (((n_x-1)*varianza_x) + ((n_y-1)*varianza_y)) / (n_x + n_y -2)
dt <- sqrt((varianza_agrupada/n_x)+(varianza_agrupada/n_y))

# Los grados de libertad 

gl <- n_x + n_y -2

# Calculamos la diferencia de medias

diferencia_medias <- media_x - media_y

alfa <- 0.05
nula <- 0
valor <- (diferencia_medias - nula) / dt
critico <- qt(p = alfa, df = gl) #Como es un contraste de cola inferior quitamos el `lower.tail = FALSE`
pvalor <- pt(valor, df = gl) #Como es un contraste de cola inferior quitamos el `lower.tail = FALSE`

# 11.47 ----

n_a <- 69
n_b <- 69

p_a <- 47/n_a
p_b <- 40/n_b

# Calculamos la diferencia de proporciones

diferencia_p <- p_a - p_b

# Calculamos la proporción desconocida

p_0 <- ((n_a*p_a)+(n_b*p_b))/(n_a + n_b)

# Calculamos la desviación típica

dt <- sqrt(((p_0*(1-p_0))/n_a) + ((p_0*(1-p_0))/n_b) )

nula <- 0
valor <- (diferencia_p - nula) / dt
pvalor <- pnorm(valor, lower.tail = FALSE)*2 #Se multiplica entre 2 porque es una alternativa bilateral (dos colas)

# 11.54 ----

Storet <- read.csv2("/cloud/project/Clases/Storet.csv", stringsAsFactors=FALSE)

t.test(x = Storet$saleb2, y = Storet$saleb4, mu = 0, alternative = "greater", var.equal = TRUE, conf.level = 0.95)

# Eliminamos el dato atípico

library(tidyverse)

Storet2 <- Storet %>%
  dplyr::filter(!saleb2 == 971)

t.test(x = Storet2$saleb2, y = Storet2$saleb4, mu = 0, alternative = "greater", var.equal = TRUE, conf.level = 0.95)
