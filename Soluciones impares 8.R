# 8.19

datos <- c(30, 42, 35, 40, 45)

n <- length(datos)
media <- mean(datos) 
s <- sd(datos)

error_tipico <- s/sqrt(5) #Desviacion estándar dividido entre la raiz cuadrada de n

gl <- n - 1
confianza <- 0.95
confianza2 <- (1-confianza)/2

tabla <- qt(p = confianza2, df = gl, lower.tail = FALSE)

ME <- tabla*(s/sqrt(n))

LI <- media - ME
LS <- media + ME

LI
LS


# 8.21

#a)

n <- 64 #Repetir con 120 y 200
s <- sqrt(144) #Repetir con 100 y 40

gl <- n - 1
confianza <- 0.98 #Repetir con 0.90 y 0.95
confianza2 <- (1-confianza)/2

tabla <- qt(p = confianza2, df = gl, lower.tail = FALSE)

ME <- tabla*(s/sqrt(n))

ME

# 8.25

n <- 9
media <- 157.82
s <- 38.89

gl <- n - 1
confianza <- 0.95
confianza2 <- (1-confianza)/2

tabla <- qt(p = confianza2, df = gl, lower.tail = FALSE)

ME <- tabla*(s/sqrt(n))

LI <- media - ME
LS <- media + ME

LI
LS

# 8.27

datos <- c(18, 25, 6, 11, 15, 20, 16, 19, 12, 17)

n <- length(datos)
media <- mean(datos) 
s <- sd(datos)

gl <- n - 1
confianza <- 0.99
confianza2 <- (1-confianza)/2

tabla <- qt(p = confianza2, df = gl, lower.tail = FALSE)

ME <- tabla*(s/sqrt(n))

LI <- media - ME
LS <- media + ME

LI
LS

# 8.29

datos <- c(16, 10, 21, 22, 8, 17, 19, 14, 19)

n <- length(datos)
media <- mean(datos) 
s <- sd(datos)

gl <- n - 1
confianza <- 0.9
confianza2 <- (1-confianza)/2

tabla <- qt(p = confianza2, df = gl, lower.tail = FALSE)

ME <- tabla*(s/sqrt(n))

LI <- media - ME
LS <- media + ME

LI
LS

# 8.31
# a)
n <- 250 #Repetir con 175 y 400
proporcion <- 0.3 #Repetir con 0.45 y 0.05
dt <- sqrt((proporcion*(1-proporcion))/n)

confianza <- 0.95 #Repetir con 0.92 y 0.96
confianza2 <- (1-confianza)/2

tabla <- qnorm(p = confianza2, lower.tail = FALSE)

ME <- tabla*dt
ME

# 8.33

n <- 142
proporcion <- 87/n
dt <- sqrt((proporcion*(1-proporcion))/n)

confianza <- 0.95
confianza2 <- (1-confianza)/2

tabla <- qnorm(p = confianza2, lower.tail = FALSE)

ME <- tabla*dt

LI <- proporcion - ME
LS <- proporcion + ME

LI
LS

# 8.39

n <- 420
proporcion <- 223/n
dt <- sqrt((proporcion*(1-proporcion))/n)

confianza <- 0.95
confianza2 <- (1-confianza)/2

tabla <- qnorm(p = confianza2, lower.tail = FALSE)

ME <- tabla*dt
ME

LI <- proporcion - ME
LS <- proporcion + ME

LI
LS

# 8.41 

#a)
n <- 246
proporcion <- (246-10-4)/n
dt <- sqrt((proporcion*(1-proporcion))/n)

confianza <- 0.98
confianza2 <- (1-confianza)/2

tabla <- qnorm(p = confianza2, lower.tail = FALSE)

ME <- tabla*dt

LI <- proporcion - ME
LS <- proporcion + ME

LI
LS

#b)
n <- 246
proporcion <- 10/n
dt <- sqrt((proporcion*(1-proporcion))/n)

confianza <- 0.98
confianza2 <- (1-confianza)/2

tabla <- qnorm(p = confianza2, lower.tail = FALSE)

ME <- tabla*dt

LI <- proporcion - ME
LS <- proporcion + ME

LI
LS

# 8.43

n <- 16
media <- 150
s <- 12

gl <- n - 1
confianza <- 0.95
confianza2 <- (1-confianza)/2

tabla <- qt(p = confianza2, df = gl, lower.tail = FALSE)

ME <- tabla*(s/sqrt(n))

LI <- media - ME
LS <- media + ME

LI
LS

# 8.57

#a)
n <- 500
proporcion <- 160/n
dt <- sqrt((proporcion*(1-proporcion))/n)

confianza <- 0.9
confianza2 <- (1-confianza)/2

tabla <- qnorm(p = confianza2, lower.tail = FALSE)

ME <- tabla*dt

LI <- proporcion - ME
LS <- proporcion + ME

LI
LS

#b)
n <- 500
proporcion <- (500-200-160)/n
dt <- sqrt((proporcion*(1-proporcion))/n)

confianza <- 0.95
confianza2 <- (1-confianza)/2

tabla <- qnorm(p = confianza2, lower.tail = FALSE)

ME <- tabla*dt

LI <- proporcion - ME
LS <- proporcion + ME

LI
LS

