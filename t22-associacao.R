setwd("E:/dev/MachineLearningR")

library(arules)
library(datasets)

# O csv nao possui header, por isso o header = FALSE
musculacao <- read.transactions("2 - Musculacao - Dados.csv", 
                                header = FALSE, sep = ";")

set.seed(202462)

inspect(musculacao)

itemFrequencyPlot(musculacao, topN = 10, type = "absolute")
rules <- apriori(musculacao,
                 parameter = list(supp = 0.2, conf = 0.85, target="rules"))

inspect(sort(rules, by = "confidence"))
