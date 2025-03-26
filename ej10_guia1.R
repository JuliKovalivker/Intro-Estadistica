# Función para calcular la probabilidad de NO capturar el tanque con número de serie N
prob_no_capturar_N <- function(N, k) {
  return((N - k) / N)
}

# Función para calcular la probabilidad de capturar al menos uno de los tanques 1 o N
prob_capturar_1_o_N <- function(N, k) {
  choose_N_k <- choose(N, k)
  choose_N2_k <- choose(N - 2, k)
  return(1 - (choose_N2_k / choose_N_k))
}

# Análisis de la probabilidad en función de N (con k = 5)
N_values <- seq(10, 200, by=10)
prob_no_N <- sapply(N_values, function(N) prob_no_capturar_N(N, 5))
prob_1_o_N <- sapply(N_values, function(N) prob_capturar_1_o_N(N, 5))

# Gráfica de probabilidad en función de N
plot(N_values, prob_no_N, type="o", col="blue", pch=16, ylim=c(0,1), 
     ylab="Probabilidad", xlab="N", main="Cambio de Probabilidad con N")
lines(N_values, prob_1_o_N, type="o", col="red", pch=16)
legend("topright", legend=c("P(No capturar N)", "P(Capturar 1 o N)"), col=c("blue", "red"), pch=16)

# Análisis de la probabilidad en función de k (con N = 100)
k_values <- seq(1, 50, by=1)
prob_no_N_k <- sapply(k_values, function(k) prob_no_capturar_N(100, k))
prob_1_o_N_k <- sapply(k_values, function(k) prob_capturar_1_o_N(100, k))

# Gráfica de probabilidad en función de k
plot(k_values, prob_no_N_k, type="o", col="blue", pch=16, ylim=c(0,1),
     ylab="Probabilidad", xlab="k", main="Cambio de Probabilidad con k (N=100)")
lines(k_values, prob_1_o_N_k, type="o", col="red", pch=16)
legend("topright", legend=c("P(No capturar N)", "P(Capturar 1 o N)"), col=c("blue", "red"), pch=16)

