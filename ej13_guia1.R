library(titanic)
data("titanic_train")

num_mujeres <- sum(titanic_train$Sex == "female")
total_pasajeros <- nrow(titanic_train)

prob_mujer <- num_mujeres / total_pasajeros
################ a ################ 
print(paste("La probabilidad de que sea mujer es:", round(prob_mujer, 4)))

num_hombres_vivos <- sum(titanic_train$Sex == "male" & titanic_train$Survived == 1)
prob_hombres_vivos <-  num_hombres_vivos / total_pasajeros
################ b ################ 
print(paste("La probabilidad de que sea hombre y alla sobrevivido es:", round(prob_hombres_vivos, 4)))


num_hombres_o_vivos <- sum(titanic_train$Sex == "male" | titanic_train$Survived == 1)
prob_hombres_o_vivos <-  num_hombres_o_vivos / total_pasajeros
################ c ################ 
print(paste("La probabilidad de que sea hombre o alla sobrevivido es:", round(prob_hombres_o_vivos, 4)))


num_tercera_clase <- sum(titanic_train$Pclass == 3)

num_ninos_tercera_clase <- sum(titanic_train$Age < 18 & titanic_train$Pclass == 3)
num_ninos_vivos_tercera_clase <- sum(titanic_train$Age < 18 & titanic_train$Survived == 1 & titanic_train$Pclass == 3)
prob_ninos_tercera_clase = num_ninos_vivos_tercera_clase / num_ninos_tercera_clase

num_mujeres_tercera_clase <- sum(titanic_train$Age >= 18 & titanic_train$Sex == "female" & titanic_train$Pclass == 3)
num_mujeres_vivas_tercera_clase <- sum(titanic_train$Age >= 18 & titanic_train$Sex == "female" & titanic_train$Survived == 1 & titanic_train$Pclass == 3)
prob_mujeres_vivas_tercera_clase = num_mujeres_vivas_tercera_clase / num_mujeres_tercera_clase

num_hombres_tercera_clase <- sum(titanic_train$Age >= 18 & titanic_train$Sex == "male" & titanic_train$Pclass == 3)
num_hombres_vivos_tercera_clase <- sum(titanic_train$Age >= 18 & titanic_train$Sex == "male" & titanic_train$Survived == 1 & titanic_train$Pclass == 3)
prob_hombres_vivos_tercera_clase = num_hombres_vivos_tercera_clase / num_hombres_tercera_clase

################ d ################ 
print("Dentro de la tercera clase:")
print(paste("La probabilidad de que sea niño y haya sobrevivido es:", round(prob_ninos_tercera_clase, 4)))
print(paste("La probabilidad de que sea mujer y haya sobrevivido es:", round(prob_mujeres_vivas_tercera_clase, 4)))
print(paste("La probabilidad de que sea hombre y haya sobrevivido es:", round(prob_hombres_vivos_tercera_clase, 4)))