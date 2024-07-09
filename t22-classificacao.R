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

classificacao_knn <- function(data, target_col) {
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

  # Matriz de confusão com base na última coluna
  confusion_knn <- confusionMatrix(predict_knn, as.factor(teste[, ncol(teste)]))
  print("Confusion Matrix:")
  print(confusion_knn)
}



print("KNN VEICULOS:")
classificacao_knn(veiculos, "tipo")
print_division()
print("KNN DIABETES")
classificacao_knn(diabetes, "diabetes")