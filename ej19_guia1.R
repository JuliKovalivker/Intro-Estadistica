############# a ############# 
lanzamientos_dado <- sample(1:6, 100, replace = TRUE)

lanzamientos_dado

############# b ############# 
sumas_dados <- replicate(100, sum(sample(1:6, 2, replace = TRUE)))

sumas_dados

############# c ############# 
simular_dados_3 <- function(n) {
  sumas <- replicate(n, sum(sample(1:6, 3, replace = TRUE)))
  return(mean(sumas == 18))  
}

frecuencia_100 <- simular_dados_3(100)
frecuencia_100000 <- simular_dados_3(100000)
frecuencia_1000000 <- simular_dados_3(1000000)

cat("Frecuencia relativa con 100 lanzamientos: ", frecuencia_100, "\n")
cat("Frecuencia relativa con 100000 lanzamientos: ", frecuencia_100000, "\n")
cat("Frecuencia relativa con 1000000 lanzamientos: ", frecuencia_1000000, "\n")

############# d ############# 
simular_dados_3_13 <- function(n) {
  sumas <- replicate(n, sum(sample(1:6, 3, replace = TRUE)))
  return(mean(sumas == 13))  
}

frecuencia_13 <- replicate(10000, sum(sample(1:6, 3, replace = TRUE)) == 13)
frecuencia_relativa_13 <- cumsum(frecuencia_13) / (1:10000)

plot(frecuencia_relativa_13, type = "l", col = "blue", xlab = "Número de lanzamientos", ylab = "Frecuencia relativa", main = "Frecuencia relativa de suma igual a 13")

############# e ############# 
frecuencia.relativa <- function(n, k) {
  frecuencia <- replicate(n, sum(sample(1:6, 3, replace = TRUE)) == k)
  frecuencia_relativa <- cumsum(frecuencia) / (1:n)
  plot(frecuencia_relativa, type = "l", col = "red", xlab = "Número de lanzamientos", ylab = "Frecuencia relativa", main = paste("Frecuencia relativa para la suma =", k))
}

frecuencia.relativa(10000, 13)

############# f ############# 
probabilidad.aproximada <- function(n) {
  sumas <- replicate(n, sum(sample(1:6, 3, replace = TRUE)))
  frecuencias <- table(sumas) / n  
  barplot(frecuencias, beside = TRUE, col = "green", xlab = "Suma", ylab = "Frecuencia relativa", main = "Frecuencia relativa de las sumas de 3 dados")
}

probabilidad.aproximada(100)
probabilidad.aproximada(1000)
probabilidad.aproximada(100000)

