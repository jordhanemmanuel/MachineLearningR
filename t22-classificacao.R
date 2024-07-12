setwd("E:/dev/MachineLearningR")

library(caret)
library(Metrics)

# Bases
veiculos <- read.csv("6 - Veiculos - Dados.csv")
veiculos$a <- NULL
diabetes <- read.csv("10 - Diabetes - Dados.csv")
diabetes$num <- NULL

veiculos_novos <- read.csv("6 - Veiculos - Novos Casos.csv")
veiculos_novos$a <- NULL
diabetes_novos <- read.csv("10 - Diabetes - Novos Casos.csv")
diabetes_novos$num <- NULL

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
  set.seed(202462)
  knn <- train(formula, data = treino, method = "knn")
  predizer_resultado(knn, teste, dataname, "KNN")
  return(knn)
}

classificacao_rna_holdout <- function(treino, teste, formula, dataname) {
  set.seed(202462)
  rna <- train(formula, data = treino, method = "nnet", trace = FALSE)
  predizer_resultado(rna, teste, dataname, "RNA Hold-out")
  return(rna)
}

classificacao_rna_cv <- function(treino, teste, formula, dataname) {
  set.seed(202462)
  ctrl <- trainControl(method = "cv", number = 10)
  rna <- train(formula, data = treino, method = "nnet",
               trace = FALSE, trControl = ctrl)
  predizer_resultado(rna, teste, dataname, "RNA Cross Validation")
  return(rna)
}

classificacao_svm_holdout <- function(treino, teste, formula, dataname) {
  set.seed(202462)
  svm <- train(formula, data = treino, method = "svmRadial")
  predizer_resultado(svm, teste, dataname, "SVM Hold-out")
  return(svm)
}

classificacao_svm_cv <- function(treino, teste, formula, dataname) {
  set.seed(202462)
  ctrl <- trainControl(method = "cv", number = 10)
  svm <- train(formula, data = treino, method = "svmRadial",
               trControl = ctrl)
  predizer_resultado(svm, teste, dataname, "SVM Cross Validation")
  return(svm)
}

classificacao_rf_holdout <- function(treino, teste, formula, dataname) {
  set.seed(202462)
  tune_grid <- expand.grid(mtry = (1:10))
  rf <- train(formula, data = treino, method = "rf", tuneGrid = tune_grid)
  predizer_resultado(rf, teste, dataname, "Random Forest Hold-out")
  return(rf)
}

classificacao_rf_cv <- function(treino, teste, formula, dataname) {
  set.seed(202462)
  ctrl <- trainControl(method = "cv", number = 10)
  rf <- train(formula, data = treino, method = "rf", trControl = ctrl)
  predizer_resultado(rf, teste, dataname, "Random Forest Cross Validation")
  return(rf)
}

classificar_todos <- function(data, target_col, dataname, newcases) {
  # Divisao 80:20
  set.seed(202462)
  indices <- createDataPartition(data[, ncol(data)], p=0.80, list=FALSE)# nolint
  treino <- data[indices, ]
  teste <- data[-indices, ]

  # Formula dinamica baseada no nome da coluna pelo parâmetro
  formula <- reformulate(".", response = target_col)

  print(paste("KNN ", dataname))
  knn <- classificacao_knn(treino, teste, formula, dataname)
  print(paste("RNA Hold-out ", dataname))
  rna_ho <- classificacao_rna_holdout(treino, teste, formula, dataname)
  print(paste("RNA Cross-validation ", dataname))
  rna_cv <- classificacao_rna_cv(treino, teste, formula, dataname)
  print(paste("SVM Hold-out ", dataname))
  svm_ho <- classificacao_svm_holdout(treino, teste, formula, dataname)
  print(paste("SVM Cross-validation ", dataname))
  svm_cv <- classificacao_svm_cv(treino, teste, formula, dataname)
  print(paste("Random Forest Hold-out ", dataname))
  rf_ho <- classificacao_rf_holdout(treino, teste, formula, dataname)
  print(paste("Random Forest Cross-validation ", dataname))
  rf_cv <- classificacao_rf_cv(treino, teste, formula, dataname)
  print_division()

  # Se 1, é veiculo entao usar SVM - Holdout, 2 é diabetes RF - Holdout
  if (newcases == 1) {
    predict_novo <- predict(svm_ho, veiculos_novos)
    resultado <- cbind(veiculos_novos, predict_novo)
    return(resultado)
  } else {
    predict_novo <- predict(rf_ho, diabetes_novos)
    resultado <- cbind(diabetes_novos, predict_novo)
    return(resultado)
  }
}

# O último parâmetro serve só para controlar qual modelo vai prever
# os novos dados
res_veiculos <- classificar_todos(veiculos, "tipo", "Veiculos", 1)
res_diabetes <- classificar_todos(diabetes, "diabetes", "Diabetes", 2)

print("PREDIÇÃO DE NOVOS CASOS:")
print(res_veiculos)
print(res_diabetes)