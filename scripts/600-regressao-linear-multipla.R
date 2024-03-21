
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
# Regressão linear
#
#-----------------------------------------------------------------------

# Dados

## Dados referentes ao consumo de combustível 
## em 48 estados norte-americanos.

## Variáveis:

### taxa: taxa do combustível no estado em USD.

### licença: proporção de motoristas licenciados.

### renda: renda percapita em USD.

### estradas: ajuda federal para as estradas 
### em mil USD.

### consumo: consumo de combustível por habitante.

### O objetivo neste estudo é explicar o consumo 
### de combustível pelas variáveis taxa, licença, 
### renda e estradas.

dados <- 
  read.csv("https://raw.githubusercontent.com/lineu96/dados-combustivel/main/combustivel.csv",
           dec = ',')

head(dados)

#-----------------------------------------------------------------------

# Análise Descritiva

## Resumo simples
summary(dados)

## Histograma das variáveis

hist(dados$cons, probability = TRUE, 
     xlab = 'Consumo', ylab = 'Densidade', 
     main = 'Consumo médio por município', 
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

## Diagramas de dispersão

plot(cons~taxa, data = dados, 
     xlab = 'Preço', 
     ylab = 'Consumo',
     main = 'Consumo x preço')

abline(lm(cons~taxa, data = dados), col=2, lwd = 2)

plot(cons~licen, data = dados, 
     xlab = 'Proporção', 
     ylab = 'Consumo',
     main = 'Consumo x prop. de licenciados')

abline(lm(cons~licen, data = dados), 
       col = 2, lwd = 2)

plot(cons~renda, data = dados, 
     xlab = 'Renda', 
     ylab = 'Consumo',
     main = 'Consumo x renda percapita')

abline(lm(cons~renda, data = dados), 
       col = 2, lwd = 2)

plot(cons~estr, data = dados, 
     xlab = 'Auxílio', 
     ylab = 'Consumo',
     main = 'Consumo x auxílio federal')

abline(lm(cons~estr, data = dados), 
       col = 2, lwd = 2)

## Correlograma
cor <- cor(dados[ ,-1])
cor

library(corrplot)
corrplot(cor,
         method = "ellipse",
         type="upper", 
         order="hclust", 
         tl.col="black", 
         tl.srt=45)

#-----------------------------------------------------------------------
# Ajuste dos Modelos de regressão
#-----------------------------------------------------------------------

## Modelo de regressão linear múltipla com todas as 
## variáveis de forma aditiva

ajuste1 <- 
  lm(cons ~ taxa + licen + renda + estr, 
     data = dados)

## Análise de resíduos
par(mfrow=c(2,2))
plot(ajuste1, which = 1:4)

## Normalidade dos resíduos
shapiro.test(ajuste1$residuals)

par(mfrow=c(1,1))
hist(ajuste1$residuals, probability = TRUE, 
     main = 'Resíduos do Ajuste',
     xlab = 'Resíduos', ylab = 'Frequência', 
     col = "#FFFFCC") # histograma dos residuos

lines(density(ajuste1$residuals), 
      col = "#666600", 
      lwd = 4)

## Medidas de influência
inf <- influence.measures(ajuste1) 
summary(inf)

#-----------------------------------------------------------------------

## Modelo de regressão linear múltipla com todas as 
## variáveis de forma aditiva sem as observações
## 19 e 44

ajuste2 <- 
  lm(cons ~ taxa+licen+renda+estr, 
     data = dados[-c(19, 44), ])

## Comparando estimativas
library(car)
compareCoefs(ajuste1, ajuste2)

## Análise de resíduos
par(mfrow=c(2,2))
plot(ajuste2, which = 1:4)

## Normalidade dos resíduos
shapiro.test(ajuste2$residuals)

hist(ajuste2$residuals, probability = TRUE, 
     main = 'Resíduos do Ajuste',
     xlab = 'Resíduos', ylab = 'Frequência', 
     col = "#FFFFCC") # histograma dos residuos

lines(density(ajuste2$residuals), 
      col = "#666600", lwd = 4)

## Resumo do Modelo Ajustado
summary(ajuste2)

## Estimativas do ajuste
round(coefficients(ajuste2),2)

## Intervalo de confiança
round(confint(ajuste2), 2)

## Usando o Modelo Ajustado para Prediçães

### Perfis
perfis <- 
  data.frame(taxa = c(7, 8), 
             licen = c(0.5, 0.6), 
             renda = c(4000, 5000),
             estr = c(3000, 5000))

### Predição de forma pontual
predict(ajuste2, newdata = perfis) 

### Intervalos de confiança (95%) 
### para o consumo médio médio.
predict(ajuste2, interval = 'confidence', 
        newdata = perfis)

### Intervalos de confiança (95%) para a predição.
predict(ajuste2, interval = 'prediction', 
        newdata = perfis)

#-----------------------------------------------------------------------
