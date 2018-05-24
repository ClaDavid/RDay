###Pre-processamento###

#Carregando dataset
data <- read.table("loja.csv",header = TRUE, sep=",")
#Informacoes do dataset
summary(data)
#Estrutura dos dados, 
#obs: int - numerico discreto, num - numerico continuo,
#Factor - categorico nominal
str(data)
#Para acesso:
data$formacao
#Para transformar em categoricoordinal
ordered(data$formacao,c("medio", "superior", "pos"))
#Removendo strings inuteis para nos
#as.numeric() = transforma char em numeric
data$salario <- as.numeric(gsub("[R$]","", data$salario))
#Podemos ver que, no caso salario, a mediana eh 6350 e a media eh 9670
#E no caso carga horaria, mediana eh 7.0 e media eh 6.6
#Podemos entao fazer o seguinte preenchimento
#is.na = checa valores not available, isto eh, ausentes
data$salario[is.na(data$salario)==TRUE] <- 6350
#visualizar salario
boxplot(data$salario)
data$carga_horaria[is.na(data$carga_horaria)==TRUE] <- 6.6
summary(data)
#Para valores inconsistentes
data$lucro[data$lucro==200000] <- mean(c(data$lucro[1], data$lucro[3:11]))
data$capacidade[data$capacidade==1] <- mean(c(data$capacidade[1:9], data$capacidade[11]))
#Analise quantitativa: converter saldo para valores numericos
#+ = + 1, - = - 1
saldo_numerico <- matrix(rep(0,11),nrow=11,ncol=1)
saldo_numerico[data$saldo=="+"] <- +1
saldo_numerico[data$saldo=="-"] <- -1
#Para lucro interessa apenas para a correlacao os sinais dos valores
#Funcao sign() = transforma valores da variavel +1 ou -1 de acordo com seu sinal
lucro = sign(data$lucro)
#Para fazermos a correlacao entre duas variaveis, podemos aplicar a funcao cor():
cor(saldo_numerico,lucro)
#Transformacao de valores numericos em categoricos
data$capacidade <- cut(data$capacidade, c(34.9, 82.5, 100), 
                       labels=c("pequena", "alta"))
#Transformacao nominal para numerico
data_formacao <- matrix(rep(0,11),nrow=11,ncol=1)
data_formacao[data$formacao=="medio"] <- 1
data_formacao[data$formacao=="superior"] <- 2
data_formacao[data$formacao=="pos"] <- 3
ferias_bits <- matrix(rep(0,11),nrow=11,ncol=1)
ferias_bits[(data$ferias=="sim"),1] <- 1
localizacao_bits <- matrix(rep(0,33), nrow=11,ncol=3)
localizacao_bits[(data$localizacao == "bairro"), 1] <- 1
localizacao_bits[(data$localizacao == "centro"), 2] <- 1
localizacao_bits[(data$localizacao == "shopping"), 3] <- 1
#Retirando atributos nao necessarios
data$ID <- NULL
#Novo conjunto de dados
data_novo <- cbind(data$Idade, data$salario, data_formacao, 
                   data$carga_horaria, data$quantidade_pessoas, 
                   ferias_bits, data$capacidade, localizacao_bits, 
                   data$tipo_area, data$lucro)
classe <- saldo_numerico
#Normalizacao min-max
min.max <- function(valor, min, max){
  valor_normalizado <- (valor - min(valor))/(max(valor) - min(valor))
  return(valor_normalizado)
}
data_novo[,1] <- min.max(data_novo[,1], 0, 1)
#Mensurar similaridade pelo uso de distancias
dist(data_novo[,1], method="euclidian")

