setwd("E:/dev/MachineLearningR")

library(caret)
library(Metrics)

# Seed fixo
set.seed(202462)

# Bases
admissao <- read.csv("9 - Admissao - Dados.csv")
admissao$num <- NULL
biomassa <- read.csv("5 - Biomassa - Dados.csv")

print_division <- function() {
  print("---------------------------------")
}

r2 <- function(observed, pred) {
  return(1 - (sum((pred - observed) ^ 2) /
                sum((pred - mean(observed)) ^ 2)))
}

syx <- function(observed, pred) {
  n <- length(observed)
  ssres <- sum((observed - pred) ^ 2)
  res <- sqrt(ssres / (n - 2))
  return(res)
}

avaliacao <- function(observed, pred) {
  # Métricas
  val_r2 <- r2(observed, pred)
  val_syx <- syx(observed, pred)
  val_pearson <- cor(observed, pred)
  val_mae <- mae(observed, pred)
  val_rmse <- rmse(observed, pred)
  print("R2:")
  print(val_r2)
  print_division()
  print("Syx:")
  print(val_syx)
  print_division()
  print("Perason:")
  print(val_pearson)
  print_division()
  print("RMSE:")
  print(val_rmse)
  print_division()
  print("MAE:")
  print(val_mae)
  print_division()
}

predizer_resultado <- function(treinado, teste, dataname, tipo_modelo) {
  print_division()
  print(paste(tipo_modelo, ":"))
  print(treinado)
  print_division()
  predict_treino <- predict(treinado, teste)
  observado <- teste[, ncol(teste)]
  avaliacao(observado, predict_treino)
}


regressao_knn <- function(treino, teste, formula, dataname) {
  # Grid com valores de K
  tune_grid <- expand.grid(k = (1:10))
  knn <- train(formula, data = treino, method = "knn", tuneGrid = tune_grid)
  predizer_resultado(knn, teste, dataname, "KNN")
}

regressao_rna_holdout <- function(treino, teste, formula, dataname) {
  rna <- train(formula, data = treino, method = "nnet",
               linout = TRUE, trace = FALSE)
  predizer_resultado(rna, teste, dataname, "RNA Hold-out")
}

regressao_rna_cv <- function(treino, teste, formula, dataname) {
  ctrl <- trainControl(method = "cv", number = 10, seeds = 202462)
  rna <- train(formula, data = treino, method = "nnet",
               linout = TRUE, trace = FALSE, trControl = ctrl)
  predizer_resultado(rna, teste, dataname, "RNA Cross Validation")
}

regressao_todos <- function(data, target_col, dataname) {
  # Divisao 80:20
  indices <- createDataPartition(data[, ncol(data)], p=0.80, list=FALSE)# nolint
  treino <- data[indices, ]
  teste <- data[-indices, ]

  # Formula dinamica baseada no nome da coluna pelo parâmetro
  formula <- reformulate(".", response = target_col)

  print(paste("KNN ", dataname))
  regressao_knn(treino, teste, formula, dataname)
  print_division()
}

print("KNN ADMISSAO")
regressao_todos(admissao, "ChanceOfAdmit", "Admissao")
print("KNN BIOMASSA:")
regressao_todos(biomassa, "biomassa", "Biomassa")
