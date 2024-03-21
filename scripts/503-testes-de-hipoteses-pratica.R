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
# AULA 7 - Análise exploratória + testes de hipótese
#-----------------------------------------------------------------------

# Leitura dos dados
dados <- 
  read.csv("https://raw.githubusercontent.com/lineu96/dados/master/dados.csv", 
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

# Tabela de frequências usando faixas de altura

breaks <- hist(dados$altura, plot = FALSE)$breaks

classes <- cut(dados$altura, 
               breaks = breaks, 
               include.lowest = TRUE, 
               right = TRUE)

tabela_altura <- table(classes)

tabela_altura <- 
  data.frame(Altura = names(tabela_altura),
             Freq = as.vector(tabela_altura),
             Freq_r = round(as.vector(prop.table(tabela_altura)), 2))

tabela_altura

tabela_altura[nrow(tabela_altura)+1,1] <- "TOTAL"

tabela_altura[nrow(tabela_altura),2] <- 
  sum(tabela_altura$Freq, na.rm = T)

tabela_altura[nrow(tabela_altura),3] <- 
  sum(tabela_altura$Freq_r, na.rm = T)

tabela_altura

# Alguns gráficos para altura
barplot(table(classes), space = 0)
hist(dados$altura)
hist(dados$altura, probability = T)
plot(density(dados$altura))
hist(dados$altura, probability = T)
lines(density(dados$altura))
boxplot(dados$altura, horizontal = T)

# Alguns resumos numéricos para altura
mean(dados$altura)
median(dados$altura)
quantile(dados$altura)
sd(dados$altura)

# Testes de hipóteses sobre a altura
t.test(dados$altura, 
       mu = 169, 
       alternative = "two.sided")

t.test(dados$altura, 
       mu = 180, 
       alternative = "two.sided")

#-----------------------------------------------------------------------

# Tabela de frequências para a variável atividade física

tabela_atividade <- table(dados$atividade_fisica)

tabela_atividade <- 
  data.frame(Atividade = names(tabela_atividade),
             Freq = as.vector(tabela_atividade),
             Freq_r = round(as.vector(prop.table(tabela_atividade)), 2))

tabela_atividade

tabela_atividade[nrow(tabela_atividade)+1,1] <- "TOTAL"

tabela_atividade[nrow(tabela_atividade),2] <- 
  sum(tabela_atividade$Freq, na.rm = T)

tabela_atividade[nrow(tabela_atividade),3] <- 
  sum(tabela_atividade$Freq_r, na.rm = T)

tabela_atividade

barplot(table(dados$atividade_fisica))

# Teste de hipótese para uma proporção.
tabela_atividade
prop.test(x = 323, n = 455, p = 0.5)

#-----------------------------------------------------------------------

# Análise da associação entre peso e atividade física
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

grupo_sim <- subset(dados, atividade_fisica == "Sim")$peso
grupo_nao <- subset(dados, atividade_fisica == "Não")$peso

var.test(grupo_sim, 
         grupo_nao, 
         alternative = "two.sided")

t.test(grupo_sim,
       grupo_nao, 
       alternative = "two.sided", 
       var.equal = FALSE)

#-----------------------------------------------------------------------

# Tabela de contingência para origem e atividade física

tabela_origem_atividade <- 
  table(dados$origem, dados$atividade_fisica)

tabela_origem_atividade

addmargins(tabela_origem_atividade)

plot(tabela_origem_atividade)

# Teste de hipótese para associação entre variáveis categóricas
chisq.test(tabela_origem_atividade)

#-----------------------------------------------------------------------

# Análise da associação entre peso e altura
dados$peso
dados$altura

# Análise gráfica
plot(peso~altura, data = dados)
coeficientes <- lm(peso~altura, data = dados)
abline(coeficientes, col = 'red')

# Correlação de Pearson
cor(dados$peso, dados$altura, method = "pearson")

# Teste para correlação
cor.test(dados$peso, dados$altura, method=c("pearson"))

#-----------------------------------------------------------------------

# Um pouco de modelagem

MASS:::fitdistr(dados$altura, densfun="normal")

modelo1 <- glm(peso ~ altura + origem, 
               family = "gaussian", 
               data = dados)

modelo1
summary(modelo1)
car::Anova(modelo1, type = 3)

#-----------------------------------------------------------------------
