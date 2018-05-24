###CLASSIFICACAO###

###Arvore de decisao###
#Sera utilizado rpart() do pacote rpart, Recursive Partitioning and
#Regression Trees, responsavel pela parte de inducao do modelo
#A parte de predicao fica com a funcao predict()
#modelo_ad <- rpart(formula, dados, metodo, controle, parametros)
#modelo_estimado <- predict(modelo_ad, exemplares_teste, tipo)
library("rpart")
servico <- read.table("servico.csv", header = TRUE, sep = ",")
data_treinamento <- servico[1:8, 1:4]
data_teste <- servico[9, 1:4]
#o uso do til (~): modelo estatistico, onde o que esta a esquerda dele eh
#a resposta e o que esta a direita dele sao as variaveis explanatorias.
#Traduzindo o que esta na formula:
#classe depende de ExpProfissionais, QualiRefeicao e LocalEstabelecimento
modelo_ad <- rpart(classe ~ ExpProfissionais + QualiRefeicao + 
                   LocalEstabelecimento, data = data_treinamento, method = "class",
                   control = rpart.control(minsplit = 1), 
                   parms = list(split = "Information"))
modelo_ad
#Explicando: 3) ExpProfissionais=POUCA 4 1 Prejuizo (0.2500000 0.7500000)
#sendo analisado no 3, com atributo ExpProfissionais=POUCA, com 4 1 indicando
#que existem 4 exemplares chegando ao no, e um deles nao pertence a classe 
#majoritaria Prejuizo. "*" indica nos folhas
#Para visualizar de forma grafica, utiliza rpart.plot()
#plot <- rpart.plot(modelo_ad, digitos, tipo)
#install.packages("rpart.plot")
library("rpart.plot")
arvore <- rpart.plot(modelo_ad, type=3)
#Predizendo
modelo_estimado <- predict(modelo_ad, data_teste, type = "class")
