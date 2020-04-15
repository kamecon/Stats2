# Intervalos de confianza para la media muestral cuando la varianza es conocida

n <- 16
media <- 25
dt <- 6

confianza <- 0.95
confianza2 <- (1-confianza)/2

tabla <- qnorm(p = confianza2, lower.tail = FALSE)

ME <- tabla*(dt/sqrt(n))

LI <- media - ME
LS <- media + ME

LI
LS

# Intervalos de confianza para la media muestral cuando la varianza es desconocida

n <- 25
media <- 50
s <- 8

gl <- n - 1
confianza <- 0.95
confianza2 <- (1-confianza)/2

tabla <- qt(p = confianza2, df = gl, lower.tail = FALSE)

ME <- tabla*(s/sqrt(n))

LI <- media - ME
LS <- media + ME

LI
LS

# Intervalos de confianza para la proporcion muestral

n <- 344
proporcion <- 261/344
dt <- sqrt((proporcion*(1-proporcion))/n)

confianza <- 0.9
confianza2 <- (1-confianza)/2

tabla <- qnorm(p = confianza2, lower.tail = FALSE)

ME <- tabla*dt

LI <- proporcion - ME
LS <- proporcion + ME

LI
LS
