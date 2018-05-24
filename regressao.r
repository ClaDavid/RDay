###REGRESSAO###

#Regressao linear simples
data <- read.table("regressaoSimplesLinear2.csv", header = TRUE, sep = ",")
plot(data$Empregados, data$Populacao)
#Utilizaremos a funcao lm(): modelo_linear <- lm(formula, dados)
x <- data$Empregados
y <- data$Populacao
modelo_linear <- lm(y~x, data)
modelo_linear
#O modelo pode ser utilizado na predicao, com predict()
#modelo_estimado <- predict(modelo_linear, exemplar_teste)
exemplar_teste <- data.frame(x=data[,2])
modelo_estimado <- predict(modelo_linear, exemplar_teste)
#Vendo graficamente
plot(x, y, xlab="Empregados", ylab="Populacao", type="p", col="blue", pch=1)
points(x, modelo_estimado, type = "p", col="red", pch = 16)
legend(locator(1), c("y", "modelo_estimado"), pch=c(1,16), col=c("blue", "red"))
#Executando novo teste
exemplar_teste <- data.frame(x = 72000)
modelo_estimado <- predict(modelo_linear, exemplar_teste)


