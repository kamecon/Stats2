# Distribución normal:
# qnorm: le das una probabilidad (por ejemplo 5%), te va a dar es el valor tal que
# a la derecha o a la izquierda tengas esa probabilidad
qnorm(0.05) # Número tal que el 5% de probabilidad está a la izquierda
qnorm(0.05, lower.tail = FALSE) # Número tal que el 5% de probabilidad está a la derecha

# pnorm: le das un número y te da la probabilidad a la izquierda o a la derecha
pnorm(1.83) # La probabilidad a la izquierda
pnorm(1.83, lower.tail = FALSE) # La probabilidad a la derecha

# Distribución t (familia de distribuciones indexado por los grados de libertad):
# qt: le das una probabilidad (por ejemplo 5%), te va a dar es el valor tal que
# a la derecha o a la izquierda tengas esa probabilidad
qt(0.05, df = 18) # Número tal que el 5% de probabilidad está a la izquierda
qt(0.05, df = 18, lower.tail = FALSE) # Número tal que el 5% de probabilidad está a la derecha

# pt: le das un número y te da la probabilidad a la izquierda o a la derecha
pt(1.83, df = 18) # La probabilidad a la izquierda
pt(1.83, df = 18, lower.tail = FALSE) # La probabilidad a la derecha

# Distribución chi-cuadrado (familia de distribuciones indexado por los grados de libertad):
# qchisq: le das una probabilidad (por ejemplo 5%), te va a dar es el valor tal que
# a la derecha o a la izquierda tengas esa probabilidad
qchisq(0.05, df = 18) # Número tal que el 5% de probabilidad está a la izquierda
qchisq(0.05, df = 18, lower.tail = FALSE) # Número tal que el 5% de probabilidad está a la derecha

# pchisq: le das un número y te da la probabilidad a la izquierda o a la derecha
pchisq(27, df = 18) # La probabilidad a la izquierda
pchisq(27, df = 18, lower.tail = FALSE) # La probabilidad a la derecha
