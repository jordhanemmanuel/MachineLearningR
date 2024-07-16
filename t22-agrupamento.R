setwd("E:/dev/MachineLearningR")

library(mlbench)
library(mice)
library(klaR)

# Bases
veiculos <- read.csv("6 - Veiculos - Dados.csv")
veiculos$a <- NULL

set.seed(202462)

## Executa o cluster
cluster_results <- kmodes(veiculos, 10, iter.max = 10, weighted = FALSE)
resultado <- cbind(veiculos, cluster_results$cluster)

head10 <- head(resultado, 10)

print(head10)