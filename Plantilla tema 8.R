# Intervalos de confianza para la media muestral cuando la varianza es conocida

n <- 
media <-
dt <- 

confianza <- 
confianza2 <- (1-confianza)/2

tabla <- qnorm(p = confianza2, lower.tail = FALSE)

ME <- tabla*(dt/sqrt(n))

LI <- media - ME
LS <- media + ME

LI
LS

# Intervalos de confianza para la media muestral cuando la varianza es desconocida

n <- 
media <- 
dt <- 

gl <- n - 1
confianza <- 
confianza2 <- (1-confianza)/2

tabla <- qt(p = confianza2, df = gl, lower.tail = FALSE)

ME <- tabla*(dt/sqrt(n))

LI <- media - ME
LS <- media + ME

LI
LS

# Intervalos de confianza para la proporcion muestral

n <- 
proporcion <- /n
dt <- sqrt((proporcion*(1-proporcion))/n)

confianza <- 
confianza2 <- (1-confianza)/2

tabla <- qnorm(p = confianza2, lower.tail = FALSE)

ME <- tabla*dt

LI <- proporcion - ME
LS <- proporcion + ME

LI
LS