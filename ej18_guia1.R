cumpleanos <- sample(1:365, 50, replace = TRUE)

repetidos <- duplicated(cumpleanos)

repeticiones <- sum(repetidos)

cat("Número de repeticiones: ", repeticiones, "\n")

simular_cumpleanos <- function(n) {
  cumpleanos <- sample(1:365, n, replace = TRUE)  
  return(sum(duplicated(cumpleanos)) > 0) 
}

simulaciones_10 <- 10
simulaciones_1000 <- 1000
simulaciones_100000 <- 100000

frecuencia_10 <- mean(replicate(simulaciones_10, simular_cumpleanos(50)))
frecuencia_1000 <- mean(replicate(simulaciones_1000, simular_cumpleanos(50)))
frecuencia_100000 <- mean(replicate(simulaciones_100000, simular_cumpleanos(50)))

cat("Frecuencia relativa en 10 simulaciones: ", frecuencia_10, "\n")
cat("Frecuencia relativa en 1000 simulaciones: ", frecuencia_1000, "\n")
cat("Frecuencia relativa en 100000 simulaciones: ", frecuencia_100000, "\n")

probabilidad_teorica <- 0.9704
cat("Probabilidad teórica (inciso a): ", probabilidad_teorica, "\n")

probabilidad_todas_diferentes <- function(n) {
  prob <- 1
  for (i in 0:(n - 1)) {
    prob <- prob * (365 - i) / 365
  }
  return(prob)
}

n_minimo <- 1
while (probabilidad_todas_diferentes(n_minimo) >= 0.4) {
  n_minimo <- n_minimo + 1
}
cat("La cantidad mínima de invitados para que la probabilidad de al menos una repetición sea superior a 0.6 es:", n_minimo, "\n")
