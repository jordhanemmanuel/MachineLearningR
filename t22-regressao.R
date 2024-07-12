setwd("E:/dev/MachineLearningR")

library(caret)
library(Metrics)

# Seed fixo
set.seed(202462)

# Bases
admissao <- read.csv("9 - Admissao - Dados.csv")
admissao$num <- NULL
biomassa <- read.csv("5 - Biomassa - Dados.csv")

admissao_novos <- read.csv("9 - Admissao - Novos Casos.csv")
admissao_novos$num <- NULL
biomassa_novos <- read.csv("5 - Biomassa - Novos Casos.csv")

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
  print("Pearson:")
  print(val_pearson)
  print_division()
  print("RMSE:")
  print(val_rmse)
  print_division()
  print("MAE:")
  print(val_mae)
  print_division()
}

predizer_resultado <- function(treinado, teste, dataname, tipo_modelo, p = 0) {
  print_division()
  print(paste(tipo_modelo, ":"))
  print(treinado)
  print_division()
  predict_treino <- predict(treinado, teste)
  observado <- teste[, ncol(teste)]
  avaliacao(observado, predict_treino)
  if (p == 1) {
    residuals <- observado - predict_treino
    plot(predict_treino, residuals, main="Admissao em SVM Cross Validation",
         xlab="Preditos", ylab="Residuais")
    abline(h=0, col="red")
  }
  if (p == 2) {
    residuals <- observado - predict_treino
    plot(predict_treino, residuals, main="Biomassa em RF Hold-out",
         xlab="Preditos", ylab="Residuais")
    abline(h=0, col="red")
  }
}


regressao_knn <- function(treino, teste, formula, dataname) {
  set.seed(202462)
  knn <- train(formula, data = treino, method = "knn")
  predizer_resultado(knn, teste, dataname, "KNN")
  return(knn)
}

regressao_rna_holdout <- function(treino, teste, formula, dataname) {
  set.seed(202462)
  rna <- train(formula, data = treino, method = "nnet",
               linout = TRUE, trace = FALSE)
  predizer_resultado(rna, teste, dataname, "RNA Hold-out")
  return(rna)
}

regressao_rna_cv <- function(treino, teste, formula, dataname) {
  set.seed(202462)
  ctrl <- trainControl(method = "cv", number = 10)
  rna <- train(formula, data = treino, method = "nnet",
               linout = TRUE, trace = FALSE, trControl = ctrl)
  predizer_resultado(rna, teste, dataname, "RNA Cross Validation")
  return(rna)
}

regressao_svm_holdout <- function(treino, teste, formula, dataname) {
  set.seed(202462)
  svm <- train(formula, data = treino, method = "svmRadial")
  predizer_resultado(svm, teste, dataname, "SVM Hold-out")
  return(svm)
}

regressao_svm_cv <- function(treino, teste, formula, dataname, melhor) {
  set.seed(202462)
  ctrl <- trainControl(method = "cv", number = 10)
  svm <- train(formula, data = treino, method = "svmRadial", trControl = ctrl)
  if (melhor == 1) {
    predizer_resultado(svm, teste, dataname, "SVM Cross Validation", melhor)
  } else {
    predizer_resultado(svm, teste, dataname, "SVM Cross Validation")
  }
  return(svm)
}

regressao_rf_holdout <- function(treino, teste, formula, dataname, melhor) {
  set.seed(202462)
  rf <- train(formula, data = treino, method = "rf")
  if (melhor == 2) {
    predizer_resultado(rf, teste, dataname, "Random Forest Hold-out", melhor)
  } else {
    predizer_resultado(rf, teste, dataname, "Random Forest Hold-out")
  }
  
  return(rf)
}

regressao_rf_cv <- function(treino, teste, formula, dataname) {
  set.seed(202462)
  ctrl <- trainControl(method = "cv", number = 10)
  rf <- train(formula, data = treino, method = "rf", trControl = ctrl)
  predizer_resultado(rf, teste, dataname, "Random Forest Cross Validation")
  return(rf)
}

regressao_todos <- function(data, target_col, dataname, melhor) {
  # Divisao 80:20
  indices <- createDataPartition(data[, ncol(data)], p=0.80, list=FALSE)# nolint
  treino <- data[indices, ]
  teste <- data[-indices, ]

  # Formula dinamica baseada no nome da coluna pelo parâmetro
  formula <- reformulate(".", response = target_col)

  print(paste("KNN ", dataname))
  knn <- regressao_knn(treino, teste, formula, dataname)
  print(paste("RNA Hold-out ", dataname))
  rna_ho <- regressao_rna_holdout(treino, teste, formula, dataname)
  print(paste("RNA Cross-validation ", dataname))
  rna_cv <- regressao_rna_cv(treino, teste, formula, dataname)
  print(paste("SVM Hold-out ", dataname))
  svm_ho <- regressao_svm_holdout(treino, teste, formula, dataname)
  print(paste("SVM Cross-validation ", dataname))
  svm_cv <- regressao_svm_cv(treino, teste, formula, dataname, melhor)
  print(paste("Random Forest Hold-out ", dataname))
  rf_ho <- regressao_rf_holdout(treino, teste, formula, dataname, melhor)
  print(paste("Random Forest Cross-validation ", dataname))
  rf_cv <- regressao_rf_cv(treino, teste, formula, dataname)
  print_division()

  # Se 1, é Admissao entao usar SVM - CV, 2 é Biomassa RF - Holdout
  if (melhor == 1) {
    predict_novo <- predict(svm_cv, admissao_novos)
    resultado <- cbind(admissao_novos, predict_novo)
    return(resultado)
  } else {
    predict_novo <- predict(rf_ho, biomassa_novos)
    resultado <- cbind(biomassa_novos, predict_novo)
    return(resultado)
  }
}

print("KNN ADMISSAO")
res_admissao <- regressao_todos(admissao, "ChanceOfAdmit", "Admissao", 1)
print("KNN BIOMASSA:")
res_biomassa <- regressao_todos(biomassa, "biomassa", "Biomassa", 2)

print("PREDIÇÃO DE NOVOS CASOS:")
print(res_admissao)
print(res_biomassa)