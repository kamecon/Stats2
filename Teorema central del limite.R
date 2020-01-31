### Teorema central del limite ###

#### Distribucion normal ####

## distribución original

set.seed(145)

original <- rnorm(100, mean = 5, sd = 2)
hist(original, freq = FALSE, xlab = "", ylab = "Frecuencia Relativa", main = "Distribución original (normal)"); curve(dnorm(x, mean = mean(original), sd =sd(original)), add=TRUE, col="darkblue")

d <- 100

a <- round(mean(original), digits = 2)
b <- round(sd(original), digits = 2)
c <- d

legend("topright", 
       c(as.expression(bquote(Media == .(a))), bquote(Desviacion == .(b)), bquote(N == .(c)))
)



# muestra1 <- sample(x = original, size = 10)
# mean(muestra1)
# sd(muestra1)
# 
# muestra2 <- sample(x = original, size = 25)
# mean(muestra2)
# sd(muestra2)
# 
# muestra3 <- sample(x = original, size = 50)
# mean(muestra3)
# sd(muestra3)

## Distribución de la media muestral con muestras N=10

medias10 <- vector(length = 1000)
for (i in seq_along(medias10)) {
  medias10[i] <- mean(sample(x = original, size = 10))
}

hist(medias10, breaks = seq(2,8,.1), xlim = c(3,8), freq = FALSE, xlab = "Media Muestral", ylab = "Frecuencia Relativa", main = ""); curve(dnorm(x, mean = mean(medias10), sd =sd(medias10)), add=TRUE, col="darkblue")

d0 <- 10

a0 <- round(mean(medias10), digits = 2)
b0 <- round(sd(medias10), digits = 2)
c0 <- d0

legend("topright", 
       c(as.expression(bquote(Media == .(a0))), bquote(Desviacion == .(b0)), bquote(N == .(c0)))
)


## Distribución de la media muestral con muestras N=25

medias25 <- vector(length = 1000)
for (i in seq_along(medias25)) {
  medias25[i] <- mean(sample(x = original, size = 25))
}

hist(medias25, breaks = seq(2,8,.1), xlim = c(3,8), freq = FALSE, xlab = "Media Muestral", ylab = "Frecuencia Relativa", main = ""); curve(dnorm(x, mean = mean(medias25), sd =sd(medias25)), add=TRUE, col="darkblue")

d1 <- 25

a1 <- round(mean(medias25), digits = 2)
b1 <- round(sd(medias25), digits = 2)
c1 <- d1

legend("topright", 
       c(as.expression(bquote(Media == .(a1))), bquote(Desviacion == .(b1)), bquote(N == .(c1)))
)

## Distribución de la media muestral con muestras N=50

medias50 <- vector(length = 1000)
for (i in seq_along(medias50)) {
  medias50[i] <- mean(sample(x = original, size = 50))
}

hist(medias50, breaks = seq(2,8,.1), xlim = c(3,8), freq = FALSE, xlab = "Media Muestral", ylab = "Frecuencia Relativa", main = ""); curve(dnorm(x, mean = mean(medias50), sd =sd(medias50)), add=TRUE, col="darkblue")

d2 <- 50

a2 <- round(mean(medias50), digits = 2)
b2 <- round(sd(medias50), digits = 2)
c2 <- d2

legend("topright", 
       c(as.expression(bquote(Media == .(a2))), bquote(Desviacion == .(b2)), bquote(N == .(c2)))
)

#### Distribucion uniforme ####

## distribución original

set.seed(145)

original_uniforme <- runif(n = 100, 1, max = 10)
hist(1:10, breaks = 0:10  ,freq = FALSE, xlab = "", ylab = "Frecuencia Relativa", main = "Distribución original (uniforme)")

## Distribución de la media muestral con muestras N=10

medias10_uniforme <- vector(length = 1000)
for (i in seq_along(medias10_uniforme)) {
  medias10_uniforme[i] <- mean(sample(x = original_uniforme, size = 10))
}

hist(medias10_uniforme, breaks = seq(2,8,.1), xlim = c(3,8), freq = FALSE, xlab = "Media Muestral", ylab = "Frecuencia Relativa", main = ""); curve(dnorm(x, mean = mean(medias10_uniforme), sd =sd(medias10_uniforme)), add=TRUE, col="darkblue")

d00 <- 10

a00 <- round(mean(medias10_uniforme), digits = 2)
b00 <- round(sd(medias10_uniforme), digits = 2)
c00 <- d00

legend("topright", 
       c(as.expression(bquote(Media == .(a00))), bquote(Desviacion == .(b00)), bquote(N == .(c00)))
)


## Distribución de la media muestral con muestras N=25

medias25_uniforme <- vector(length = 1000)
for (i in seq_along(medias25_uniforme)) {
  medias25_uniforme[i] <- mean(sample(x = original_uniforme, size = 25))
}

hist(medias25_uniforme, breaks = seq(2,8,.1), xlim = c(3,8), freq = FALSE, xlab = "Media Muestral", ylab = "Frecuencia Relativa", main = ""); curve(dnorm(x, mean = mean(medias25_uniforme), sd =sd(medias25_uniforme)), add=TRUE, col="darkblue")

d11 <- 25

a11 <- round(mean(medias25_uniforme), digits = 2)
b11 <- round(sd(medias25_uniforme), digits = 2)
c11 <- d11

legend("topright", 
       c(as.expression(bquote(Media == .(a11))), bquote(Desviacion == .(b11)), bquote(N == .(c11)))
)

## Distribución de la media muestral con muestras N=50

medias50_uniforme <- vector(length = 1000)
for (i in seq_along(medias50_uniforme)) {
  medias50_uniforme[i] <- mean(sample(x = original_uniforme, size = 50))
}

hist(medias50_uniforme, breaks = seq(2,8,.1), xlim = c(3,8), freq = FALSE, xlab = "Media Muestral", ylab = "Frecuencia Relativa", main = ""); curve(dnorm(x, mean = mean(medias50_uniforme), sd =sd(medias50_uniforme)), add=TRUE, col="darkblue")

d22 <- 50

a22 <- round(mean(medias50_uniforme), digits = 2)
b22 <- round(sd(medias50_uniforme), digits = 2)
c22 <- d22

legend("topright", 
       c(as.expression(bquote(Media == .(a22))), bquote(Desviacion == .(b22)), bquote(N == .(c22)))
)

#### Distribución sesgada (Beta) ####

## distribución original

set.seed(145)

original_beta <- rbeta(n = 100, 2, 5)
hist(original_beta, breaks = seq(0,0.8,0.05) ,freq = FALSE, xlab = "", ylab = "Frecuencia Relativa", main = "Distribución original (sesgada a la derecha)")

## Distribución de la media muestral con muestras N=10

medias10_beta <- vector(length = 1000)
for (i in seq_along(medias10_beta)) {
  medias10_beta[i] <- mean(sample(x = original_beta, size = 10))
}

hist(medias10_beta,breaks = seq(0.1,0.6,0.008), xlim = c(0.1,0.6),  freq = FALSE, xlab = "Media Muestral", ylab = "Frecuencia Relativa", main = ""); curve(dnorm(x, mean = mean(medias10_beta), sd =sd(medias10_beta)), add=TRUE, col="darkblue")

d01 <- 10

a01 <- round(mean(medias10_beta), digits = 2)
b01 <- round(sd(medias10_beta), digits = 2)
c01 <- d01

legend("topright", 
       c(as.expression(bquote(Media == .(a01))), bquote(Desviacion == .(b01)), bquote(N == .(c01)))
)


## Distribución de la media muestral con muestras N=25

medias25_beta <- vector(length = 1000)
for (i in seq_along(medias25_beta)) {
  medias25_beta[i] <- mean(sample(x = original_beta, size = 25))
}

hist(medias25_beta, freq = FALSE, breaks = seq(0.1,0.6,0.008), xlim = c(0.1,0.6), xlab = "Media Muestral", ylab = "Frecuencia Relativa", main = ""); curve(dnorm(x, mean = mean(medias25_beta), sd =sd(medias25_beta)), add=TRUE, col="darkblue")

d12 <- 25

a12 <- round(mean(medias25_beta), digits = 2)
b12 <- round(sd(medias25_beta), digits = 2)
c12 <- d12

legend("topright", 
       c(as.expression(bquote(Media == .(a12))), bquote(Desviacion == .(b12)), bquote(N == .(c12)))
)

## Distribución de la media muestral con muestras N=50

medias50_beta <- vector(length = 1000)
for (i in seq_along(medias50_beta)) {
  medias50_beta[i] <- mean(sample(x = original_beta, size = 50))
}

hist(medias50_beta,  freq = FALSE, breaks = seq(0.1,0.6,0.008), xlim = c(0.1,0.6), xlab = "Media Muestral", ylab = "Frecuencia Relativa", main = ""); curve(dnorm(x, mean = mean(medias50_beta), sd =sd(medias50_beta)), add=TRUE, col="darkblue")

d21 <- 50

a21 <- round(mean(medias50_beta), digits = 2)
b21 <- round(sd(medias50_beta), digits = 2)
c21 <- d21

legend("topright", 
       c(as.expression(bquote(Media == .(a21))), bquote(Desviacion == .(b21)), bquote(N == .(c21)))
)


