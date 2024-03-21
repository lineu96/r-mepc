#----------------------------------------------------------------------
#
# MÉTODOS ESTATÍSTICOS EM PESQUISA CIENTÍFICA - MEPC
# OFERTA 2023
# APOIO COMPUTACIONAL EM LINGUAGEM R
#
#----------------------------------------------------------------------
# 
# Prof. Lineu Alberto Cavazani de Freitas
# Laboratório de Estatística e Geoinformação
# Departamento de Estatística
# Universidade Federal do Paraná
# URL: https://lineu96.github.io/
#
# AULA 2 - Análise exploratória com R
#-----------------------------------------------------------------------

dados <- read.csv("https://raw.githubusercontent.com/lineu96/dados/master/dados.csv", 
                     sep = ',', 
                     encoding = 'UTF-8', 
                     header = T)

head(dados)
ncol(dados)
nrow(dados)
dim(dados)
summary(dados)
names(dados)

#-----------------------------------------------------------------------

dados$origem

table(dados$origem)
tabela1 <- table(dados$origem)
tabela1
sum(tabela1)

prop.table(tabela1)
tabela2 <- prop.table(tabela1)
tabela2
sum(tabela2)

tabela2*100
tabela3 <- tabela2*100
tabela3
sum(tabela3)

plot(tabela1)
plot(tabela2)
plot(tabela3)

barplot(tabela1)
barplot(tabela2)
barplot(tabela3)

barplot(tabela1)
barplot(tabela2)
barplot(tabela3)

pie(tabela1)
pie(tabela2)
pie(tabela3)

tabela4 <- table(dados$origem, rep(1,nrow(dados)))
barplot(tabela4)
barplot(tabela4, horiz = T)

tabela5 <- prop.table(tabela4)
barplot(tabela4)
barplot(tabela4, horiz = T)

tabela6 = data.frame(niveis = names(tabela1),
                     freq = as.vector(tabela1),
                     freq_r = as.vector(tabela2))

tabela6
tabela6[3,1] <- "TOTAL"
tabela6[3,2] <- sum(tabela6$freq, na.rm = T)
tabela6[3,3] <- sum(tabela6$freq_r, na.rm = T)
tabela6

#-----------------------------------------------------------------------

dados$altura

breaks <- hist(dados$altura, plot = FALSE)$breaks

classes <- cut(dados$altura, 
               breaks = breaks, 
               include.lowest = TRUE, 
               right = TRUE)

table(classes)
tabela15 <- table(classes)
tabela15
sum(tabela15)

prop.table(tabela15)
tabela16 <- prop.table(tabela15)
tabela16
sum(tabela16)

tabela16*100
tabela17 <- tabela16*100
tabela17
sum(tabela17)

barplot(tabela15, space = 0)
hist(dados$altura)
hist(dados$altura, probability = T)

densidade <- density(dados$altura)
plot(densidade)

hist(dados$altura, probability = T)
lines(densidade)

mean(dados$altura)
median(dados$altura)

tabela_alturas <- as.data.frame(table(dados$altura))
subset(tabela_alturas, Freq == max(tabela_alturas$Freq))

quantile(dados$altura)

aic <- quantile(dados$altura)[4] - quantile(dados$altura)[2]

boxplot(dados$altura)
boxplot(dados$altura, horizontal = T)

quantile(dados$altura, seq(0,1,0.1))

var(dados$altura)
sd(dados$altura)

range(dados$altura)
diff(range(dados$altura))

cv <- (sd(dados$altura)/mean(dados$altura))*100
cv

desvios <- dados$altura - mean(dados$altura)
desvios
mean(desvios)
sd(desvios)

escores <- (dados$altura-mean(dados$altura))/ sd(dados$altura)
escores
mean(escores)
sd(escores)

#-----------------------------------------------------------------------

dados$origem
dados$atividade_fisica

table(dados$origem, dados$atividade_fisica)
tabela7 <- table(dados$origem, 
                 dados$atividade_fisica)
tabela7
sum(tabela7)

prop.table(tabela7)
tabela8 <- prop.table(tabela7)
tabela8
sum(tabela8)

tabela8*100
tabela9 <- tabela8*100
tabela9
sum(tabela9)

plot(tabela7)
plot(tabela8)
plot(tabela9)

barplot(tabela7, legend.text = rownames(tabela7), beside = T)
barplot(tabela8, legend.text = rownames(tabela8), beside = T)
barplot(tabela9, legend.text = rownames(tabela8), beside = T)

barplot(t(tabela7), legend.text = colnames(tabela7), beside = T)
barplot(t(tabela8), legend.text = colnames(tabela8), beside = T)
barplot(t(tabela9), legend.text = colnames(tabela9), beside = T)

barplot(tabela7, legend.text = rownames(tabela7))
barplot(tabela8, legend.text = rownames(tabela8))
barplot(tabela9, legend.text = rownames(tabela8))

barplot(t(tabela7), legend.text = colnames(tabela7))
barplot(t(tabela8), legend.text = colnames(tabela8))
barplot(t(tabela9), legend.text = colnames(tabela9))

barplot(tabela7, horiz=T, legend.text = rownames(tabela7))
barplot(tabela8, horiz=T, legend.text = rownames(tabela8))
barplot(tabela9, horiz=T, legend.text = rownames(tabela9))

barplot(t(tabela7), horiz=T, legend.text = colnames(tabela7))
barplot(t(tabela8), horiz=T, legend.text = colnames(tabela8))
barplot(t(tabela9), horiz=T, legend.text = colnames(tabela9))

total_linha <- margin.table(tabela7,2)
total_coluna <- margin.table(tabela7,1)
total_linha
total_coluna

addmargins(tabela7)

#-----------------------------------------------------------------------

prop.table(tabela7, margin = 1)
tabela11 <- prop.table(tabela7, margin = 1)
tabela11
sum(tabela11)

tabela11[1,]
sum(tabela11[1,])

tabela11[2,]
sum(tabela11[2,])

barplot(t(tabela11), legend.text = colnames(tabela11))

total_linha <- margin.table(tabela11,2)
total_coluna <- margin.table(tabela11,1)
total_linha
total_coluna

addmargins(tabela11)

#-----------------------------------------------------------------------

prop.table(tabela7, margin = 2)
tabela13 <- prop.table(tabela7, margin = 2)
tabela13
sum(tabela13)

tabela13[,1]
sum(tabela13[,1])

tabela13[,2]
sum(tabela13[,2])

barplot(tabela13, legend.text = row.names(tabela13))

total_linha <- margin.table(tabela13,2)
total_coluna <- margin.table(tabela13,1)
total_linha
total_coluna

addmargins(tabela13)

#-----------------------------------------------------------------------

dados$peso
dados$altura

plot(peso~altura, data = dados)
lm(peso~altura, data = dados)
coeficientes <- lm(peso~altura, data = dados)
abline(coeficientes, col = 'red')

cor(dados$peso, dados$altura, method = "pearson")
cor(dados$peso, dados$altura, method = "kendall")
cor(dados$peso, dados$altura, method = "spearman")

#-----------------------------------------------------------------------

dados$peso
dados$atividade_fisica

tapply(X = dados$peso, 
       INDEX = dados$atividade_fisica, 
       FUN = mean)

tapply(X = dados$peso, 
       INDEX = dados$atividade_fisica, 
       FUN = sd)

tapply(X = dados$peso, 
       INDEX = dados$atividade_fisica, 
       FUN = summary)

boxplot(peso~atividade_fisica, data = dados)

grupo1 <- subset(dados, atividade_fisica == "Sim")
grupo2 <- subset(dados, atividade_fisica == "Não")

hist(grupo1$peso)
hist(grupo2$peso)

plot(density(grupo1$peso))
lines(density(grupo2$peso), col = 2)

#-----------------------------------------------------------------------