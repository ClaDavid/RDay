###AGRUPAMENTO###

min.max <- function(valor, min, max){
  valor_normalizado <- (valor - min(valor))/(max(valor) - min(valor))
  return(valor_normalizado)
}


###K-Means
data <- read.table("globalsesAgrupamento.csv", header = TRUE, sep = ",")
data$ses <- min.max(data$ses,0,1)
data$gdppc <- min.max(data$gdppc,0,1)
data$yrseduc <- min.max(data$yrseduc,0,1)
#funcao kmeans() do pacote stats
#kmeans(dados, k, iteracoes)
status <- kmeans(data, 2, iter.max = 5)
perfis_ses <- status$cluster
#visualizando graficamente
#install.packages("cluster")
library(cluster)
clusplot(data, perfis_ses, color=TRUE, lines = 0)

###Fuzzy C-Means
library("e1071")
statusFuzzy <- cmeans(data, 2)
statusFuzzy
#vendo graficamente
matplot(statusFuzzy$membership, type = "l", lty = 1, 
        xlab = "Exemplos", ylab = "Grau de pertinencia")




