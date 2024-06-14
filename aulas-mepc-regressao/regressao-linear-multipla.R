
#----------------------------------------------------------------------
#
# MÉTODOS ESTATÍSTICOS EM PESQUISA CIENTÍFICA - MEPC
#
# REGRESSÃO LINEAR
#
#----------------------------------------------------------------------
# 
# Prof. Lineu Alberto Cavazani de Freitas
# Laboratório de Estatística e Geoinformação
# Departamento de Estatística
# Universidade Federal do Paraná
# URL: https://lineu96.github.io/
#
#-----------------------------------------------------------------------

# Dados:

## Dados referentes ao consumo de combustível 
## em 48 estados norte-americanos.

#-----------------------------------------------------------------------

# Variáveis:

## taxa: taxa do combustível no estado em USD.
## licen: proporção de motoristas licenciados.
## renda: renda percapita em USD.
## estr: ajuda federal para as estradas em mil USD.
## cons: consumo de combustível por habitante.

#-----------------------------------------------------------------------

# Objetivo:

## Explicar o consumo de combustível pelas 
## variáveis taxa, licença, renda e ajuda nas estradas.

#-----------------------------------------------------------------------

# Leitura dos dados

dados <- 
  read.csv("https://raw.githubusercontent.com/lineu96/dados-combustivel/main/combustivel.csv",
           dec = ',')

head(dados)

#-----------------------------------------------------------------------
# Análise Descritiva
#-----------------------------------------------------------------------
## Resumo simples
summary(dados)
#-----------------------------------------------------------------------
## Histograma das variáveis

hist(dados$cons, probability = TRUE, 
     xlab = 'Consumo', ylab = 'Densidade', 
     main = 'Consumo médio', 
     col = "#FFCCCC", 
     ylim = c(0, 0.0045))

lines(density(dados$cons), col = "#990000", lwd = 4)

hist(dados$taxa, probability = TRUE, 
     xlab = 'Preço', ylab = 'Densidade', 
     main = 'Preço do combustível', 
     col = "#99FF99", ylim = c(0, 0.5))

lines(density(dados$taxa), col = "#006600", lwd = 4)

hist(dados$licen, probability = TRUE, 
     xlab = 'Proporção', ylab = 'Densidade', 
     main = 'Prop. de licenciados', 
     col = "#CCFFFF", ylim = c(0, 8))

lines(density(dados$licen), col = "#000099", lwd = 4)

hist(dados$renda, probability = TRUE, 
     xlab = 'Renda', ylab = 'Densidade', 
     main = 'Renda percapita', col = "#FFCCFF")

lines(density(dados$renda), col = "#663366", lwd = 4)

hist(dados$estr, probability = TRUE, 
     xlab = 'Auxílio', ylab = 'Densidade', 
     main = 'Auxílio federal', col = "#E6E8FA", xlim = c(0, +15000))

lines(density(dados$estr), col = "#545454", lwd = 4)

#-----------------------------------------------------------------------
## Diagramas de dispersão

plot(cons~taxa, data = dados, 
     xlab = 'Preço', 
     ylab = 'Consumo',
     main = 'Consumo x preço',
     pch = 19)

abline(lm(cons~taxa, data = dados), col=2, lwd = 2)

plot(cons~licen, data = dados, 
     xlab = 'Proporção', 
     ylab = 'Consumo',
     main = 'Consumo x prop. de licenciados',
     pch = 19)

abline(lm(cons~licen, data = dados), 
       col = 2, lwd = 2)

plot(cons~renda, data = dados, 
     xlab = 'Renda', 
     ylab = 'Consumo',
     main = 'Consumo x renda percapita',
     pch = 19)

abline(lm(cons~renda, data = dados), 
       col = 2, lwd = 2)

plot(cons~estr, data = dados, 
     xlab = 'Auxílio', 
     ylab = 'Consumo',
     main = 'Consumo x auxílio federal',
     pch = 19)

abline(lm(cons~estr, data = dados), 
       col = 2, lwd = 2)

#-----------------------------------------------------------------------
## Correlograma
cor <- cor(dados)
round(cor,2)

library(corrplot)
corrplot(cor, 
         type = 'lower', 
         order = 'hclust', 
         tl.col = 'black',
         cl.ratio = 0.2, 
         tl.srt = 45,
         method = "ellipse")

#-----------------------------------------------------------------------
# Ajuste dos Modelos de regressão
#-----------------------------------------------------------------------

## Primeira estratégia: modelo de regressão linear 
## múltipla com todas as variáveis de forma aditiva

ajuste1 <- 
  lm(cons ~ taxa + licen + renda + estr, 
     data = dados)

## Análise de resíduos
par(mfrow=c(2,2))
plot(ajuste1, which = 1:4)

#-----------------------------------------------------------------------

## Segunda estratégia: modelo de regressão linear 
## múltipla com todas as variáveis de forma aditiva
## sem as observações 19 e 44

ajuste2 <- 
  lm(cons ~ taxa+licen+renda+estr, 
     data = dados[-c(44, 3), ])

## Análise de resíduos
par(mfrow=c(2,2))
plot(ajuste2, which = 1:4)

## Comparando estimativas
library(car)
compareCoefs(ajuste1, ajuste2)

## Medidas de qualidade de ajuste
data.frame(AIC = c(AIC(ajuste1), AIC(ajuste2)),
           BIC = c(BIC(ajuste1), BIC(ajuste2)),
           logLik = c(logLik(ajuste1), logLik(ajuste2)))

#-----------------------------------------------------------------------
# Usando o segundo ajuste para interpretações

## Resumo do Modelo Ajustado
summary(ajuste2)

## Estimativas do ajuste
round(coefficients(ajuste2),2)

## Intervalo de confiança
round(confint(ajuste2), 2)

#-----------------------------------------------------------------------

# Usando o segundo ajuste para prediçães

## Perfis
perfis <- 
  data.frame(taxa = c(7, 8), 
             licen = c(0.5, 0.6), 
             renda = c(4000, 5000),
             estr = c(3000, 5000))

## Predição de forma pontual
predict(ajuste2, newdata = perfis) 

## Intervalos de confiança (95%) 
## para o consumo médio médio.
predict(ajuste2, interval = 'confidence', 
        newdata = perfis)

## Intervalos de confiança (95%) 
## para a predição.
predict(ajuste2, interval = 'prediction', 
        newdata = perfis)

#-----------------------------------------------------------------------
