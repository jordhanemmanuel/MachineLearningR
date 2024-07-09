setwd("C:/Users/Jordhan/Desktop/POS/Trabalho IAA008 - Equipe 22")

library(caret)
library(Metrics)

# Seed fixo
set.seed(202462)

# Bases
veiculos <- read.csv("6 - Veiculos - Dados.csv")
veiculos$a <- NULL
diabetes <- read.csv("10 - Diabetes - Dados.csv")
diabetes$num <- NULL

print_division <- function() {
  print("---------------------------------")
}

r2 <- function(predito, observado) {
  return(1 - (sum((predito - observado) ^ 2) /
                sum((observado - mean(observado)) ^ 2)))
}

regressao_knn <- function(data, target_col) {
  # Divisao 80/20
  ran <- sample(1:nrow(data), 0.8 * nrow(data))
  treino <- data[ran, ]
  teste <- data[-ran, ]

  # Criação da formula dinamica
  formula <- reformulate(".", response = target_col)

  # Grid com valores de K
  tune_grid <- expand.grid(k = (1:10))
  knn <- train(formula, data = treino, method = "knn", tuneGrid = tune_grid)
  print_division()
  print("KNN:")
  print(knn)
  print_division()
  predict_knn <- predict(knn, teste)

  # COLOCAR AQUI QUAL COLUNA
  observado <- teste[, ncol(teste)]

  # Métricas
  val_rmse <- rmse(observado, predict_knn)
  val_r2 <- r2(predict_knn, observado)

  print("RMSE:")
  print(val_rmse)
  print_division()
  print("R2:")
  print(val_r2)
  print_division()
}



print("KNN VEICULOS:")
classificacao_knn(veiculos, "tipo")
print_division()
print("KNN DIABETES")
classificacao_knn(diabetes, "diabetes")