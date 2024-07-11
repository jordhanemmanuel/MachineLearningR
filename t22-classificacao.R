setwd("E:/dev/MachineLearningR")

library(caret)
library(Metrics)

# Bases
veiculos <- read.csv("6 - Veiculos - Dados.csv")
veiculos$a <- NULL
diabetes <- read.csv("10 - Diabetes - Dados.csv")
diabetes$num <- NULL

print_division <- function() {
  print("---------------------------------")
}

print_matrix <- function(observed, pred, title, dataname) {
  # Matriz de confusão com base na última coluna
  confusion_knn <- confusionMatrix(pred, as.factor(observed))
  print(paste(dataname, title, "Confusion Matrix ", ":"))
  print(confusion_knn)
}

predizer_resultado <- function(treinado, teste, dataname, tipo_modelo) {
  print_division()
  print(paste(tipo_modelo, ":"))
  print(treinado)
  print_division()
  predict_treino <- predict(treinado, teste)
  observado <- teste[, ncol(teste)]
  print_matrix(observado, predict_treino, tipo_modelo, dataname)
}

classificacao_knn <- function(treino, teste, formula, dataname) {
  # Grid com valores de K
  set.seed(202462)
  tune_grid <- expand.grid(k = (1:10))
  knn <- train(formula, data = treino, method = "knn", tuneGrid = tune_grid)
  predizer_resultado(knn, teste, dataname, "KNN")
}

classificacao_rna_holdout <- function(treino, teste, formula, dataname) {
  set.seed(202462)
  rna <- train(formula, data = treino, method = "nnet", trace = FALSE)
  predizer_resultado(rna, teste, dataname, "RNA Hold-out")
}

classificacao_rna_cv <- function(treino, teste, formula, dataname) {
  set.seed(202462)
  ctrl <- trainControl(method = "cv", number = 10)
  rna <- train(formula, data = treino, method = "nnet",
               trace = FALSE, trControl = ctrl)
  predizer_resultado(rna, teste, dataname, "RNA Cross Validation")
}

classificar_todos <- function(data, target_col, dataname) {
  # Divisao 80:20
  set.seed(202462)
  indices <- createDataPartition(data[, ncol(data)], p=0.80, list=FALSE)# nolint
  treino <- data[indices, ]
  teste <- data[-indices, ]

  # Formula dinamica baseada no nome da coluna pelo parâmetro
  formula <- reformulate(".", response = target_col)

  print(paste("KNN ", dataname))
  classificacao_knn(treino, teste, formula, dataname)
  print(paste("RNA Hold-out ", dataname))
  classificacao_rna_holdout(treino, teste, formula, dataname)
  print(paste("RNA Cross-validation ", dataname))
  classificacao_rna_cv(treino, teste, formula, dataname)
  print_division()
}

classificar_todos(veiculos, "tipo", "Veiculos")
classificar_todos(diabetes, "diabetes", "Diabetes")