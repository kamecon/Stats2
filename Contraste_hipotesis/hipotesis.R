library(moderndive)
library(tidyverse)
library(patchwork)

# El siguiente ejemplo es sacado del libro "Statistical Inference via Data Science: A ModernDive into R and the tidyverse" https://moderndive.com/index.html usamos la librería `moderndive` del libro para cargar los datos que vamos a analizar

# Vamos a analizar datos de un expermiento publicado en 1974, en el cual 48 supervisores de un banco, asumiendo una posición 

View(promotions)
promotions %>%
  group_by(gender, decision) %>%
  tally()

genero1 <- ggplot(promotions,
aes(x = gender, fill = decision)) +
geom_bar() +
labs(x = "Género del CV")

genero1

# Hacemos una copia de la tabla `promotions`
promociones <- promotions

# Aleatorizamos la columna de género, de ese modo podemos intentar replicar que pasaría en un mundo donde la variable género no juega ningún rol en la decisión de promoción
promociones$gender <- sample(x = promociones$gender)

# Verificamos que las tablas no son iguales
View(promociones)

# Vemos graficamente la diferencia

genero2 <- ggplot(promociones,
                  aes(x = gender, fill = decision)) +
  geom_bar() +
  labs(x = "Género del CV")

genero1 + genero2


promociones2 <- promociones %>%
  group_by(gender, decision) %>%
  tally() %>%
  mutate(prop = n/24) %>%
  dplyr::filter(decision == "promoted") %>% 
  ungroup() %>% 
  arrange(desc(gender)) %>% 
  mutate(diferencia = prop - dplyr::lag(prop))
promociones2
promociones2$diferencia[2]

#Repetimos lo anterior 1000 veces

promociones <- promotions
promociones_aleatorio <- promociones
diferencias <- vector(length = 1000)
for (i in seq_along(diferencias)) {
  promociones_aleatorio$gender <- sample(x = promociones$gender)
  promociones2 <- promociones_aleatorio %>%
    group_by(gender, decision) %>%
    tally() %>%
    mutate(prop = n/24) %>%
    dplyr::filter(decision == "promoted") %>% 
    ungroup() %>% 
    mutate(diferencia = prop - dplyr::lag(prop))
  diferencias[i] <- promociones2$diferencia[2]
}

ggplot(as.data.frame(diferencias), aes(x=diferencias))+geom_histogram(binwidth=.085, fill="cornsilk", colour="grey60") +
  geom_vline(aes(xintercept=0.2916667), color="red", linetype="dashed", size=1) 

#+  geom_vline(aes(xintercept=mean(diferencias) - 1.96*sd(diferencias)), color="black", size=1) +
#  geom_vline(aes(xintercept=mean(diferencias) + 1.96*sd(diferencias)), color="black", size=1) 

round(diferencias, digits = 2) %>% table() %>% prop.table()

promotions$decision <- sample(promotions$decision, size = nrow(promociones), replace = TRUE)

# Bootsrapping

promo_prueba <- promotions
promo_alea <-promo_prueba[sample(1:nrow(promotions), size = nrow(promotions), replace = TRUE), ]
promo_alea %>%
group_by(gender, decision) %>%
tally()

dif_boot <- vector(length = 1000)
for (i in seq_along(dif_boot)) {
  promo_alea <-promo_prueba[sample(1:nrow(promotions), size = nrow(promotions), replace = TRUE), ]
  promo_boot <- promo_alea %>%
    group_by(gender, decision) %>%
    tally() %>%
    mutate(prop = n/24) %>%
    dplyr::filter(decision == "promoted") %>% 
    ungroup() %>% 
    arrange(desc(gender)) %>% 
    mutate(diferencia = prop - dplyr::lag(prop))
  dif_boot[i] <- promo_boot$diferencia[2]
}

quantile(dif_boot, c(0.025, 0.975))
