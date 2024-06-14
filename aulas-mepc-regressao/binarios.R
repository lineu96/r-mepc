
#----------------------------------------------------------------------
#
# MÉTODOS ESTATÍSTICOS EM PESQUISA CIENTÍFICA - MEPC
#
# Modelos de Regressão para Dados Binários
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

# Dados

## Exemplo disponível em 
## Bennett, K.,P., and Mangasarian, O.L., 1992

## Os dados provêm de um estudo de câncer de mama em 
## Wisconsin. 

## São 681 casos de tumores potencialmente cancerosos, 
## dos quais 238 são realmente malignos.

## As variáveis explicativas disponíveis dizem 
## respeito a características observadas nos 
## tumores sob estudo.

## Os valores das variáveis preditoras foram 
## determinados por um médico observando as células e 
## classificando-as em uma escala de 1 (normal) a 10 
## (mais anormal) em relação a cada característica 
## avaliada.

#-----------------------------------------------------------------------

# Variáveis

## Class - classificação do tumor: 0 maligno, 1 benigno (resposta)
## Adhes - adesão marginal
## BNucl - nucléolos nus
## Chrom - cromatina branda
## Epith - tamanho da célula epitelial
## Mitos - mitose
## NNucl - nucléolo normal
## Thick - espessura do aglomerado
## UShap - uniformidade da forma da célula
## USize - uniformidade do tamanho da célula

#-----------------------------------------------------------------------

## O objetivo da análise é modelar a probabilidade do 
## tumor sob investigação ser maligno.

## Os dados estão disponíveis no pacote de 
## Julian Faraway (2016). 

## O pacote fornece a documentação dos dados. Verifique!

#-----------------------------------------------------------------------

library(faraway)

dados <- wbca
View(dados)
head(dados)

#-----------------------------------------------------------------------

## Verificando a quantidade de tumores 
## de cada tipo
table(dados$Class)
prop.table(table(dados$Class))

#-----------------------------------------------------------------------

## Vamos redefinir os níveis da variável classe de 
## forma a modelar a probabilidade de tumor maligno:
dados$Class <- ifelse(dados$Class == 0, 1, 0)
table(dados$Class)

#-----------------------------------------------------------------------

# Análise exploratória

## Relação das explicativas com os níveis da resposta

library(ggplot2)
library(gridExtra)

bp <- function(covariavel, xlab){
  ggplot(dados, 
         aes(x=factor(Class), 
             y=covariavel, 
             color=factor(Class))) + 
    geom_boxplot()+ 
    guides(color=FALSE)+
    xlab(xlab)+ 
    ylab('') +
    theme_light()
}

g1 <- bp(dados[,2],names(dados[2]))
g2 <- bp(dados[,3],names(dados[3]))
g3 <- bp(dados[,4],names(dados[4]))
g4 <- bp(dados[,5],names(dados[5]))
g5 <- bp(dados[,6],names(dados[6]))
g6 <- bp(dados[,7],names(dados[7]))
g7 <- bp(dados[,8],names(dados[8]))
g8 <- bp(dados[,9],names(dados[9]))
g9 <- bp(dados[,10],names(dados[10]))

grid.arrange(g1, g2, g3, 
             g4, g5, g6, 
             g7, g8, g9, 
             ncol=3, nrow=3)

#-----------------------------------------------------------------------

# Ajuste dos modelos

## Vamos explorar 4 modelos e verificar qual deles perfoma melhor

## Todos os 4 modelos terão a mesma estrutura, que considera 
## todas as explicativas disponíveis de forma aditiva

## O que muda de um modelo para o outro é a função de ligação

logit <- glm(Class ~ .,
             family=binomial(link='logit'),
             data = dados)

probit <- glm(Class ~ .,
              family=binomial(link = 'probit'),
              data = dados)

cloglog <- glm(Class ~ .,
               family=binomial(link='cloglog'),
               data = dados)

cauchit <- glm(Class ~ .,
               family=binomial(link='cauchit'),
               data = dados)

#-----------------------------------------------------------------------

# Escolha do Modelo via AIC

selec <- 
  data.frame(ajuste=c('logito', 'probito', 'cloglog', 'cauchy'),
             aic=c(AIC(logit), AIC(probit), 
                   AIC(cloglog), AIC(cauchit)),
             logLik=c(logLik(logit),logLik(probit),
                      logLik(cloglog),logLik(cauchit)))

selec

#-----------------------------------------------------------------------

# Análise do modelo ajustado selecionado
summary(probit)

# Reajuste do Modelo
probit2 <- step(probit, direction = "both")
summary(probit2)

# Comparando os modelos
anova(probit, probit2, test = 'Chisq')

#-----------------------------------------------------------------------

# Resíduos Quantílicos Aleatorizados

library(statmod)
par(mfrow=c(1,2))
residuos <- qresiduals(probit)
plot(residuos)
qqnorm(residuos)
qqline(residuos, col = 2)

#-----------------------------------------------------------------------

# Predição
perfis <- data.frame(Adhes = c(6,  3), 
                     BNucl = c(9,  1),
                     Chrom = c(3,  7),
                     Mitos = c(9,  1),
                     NNucl = c(8,  3),
                     Thick = c(2,  4),
                     UShap = c(6,  5)
)

predict(probit2, 
        interval = 'prediction', 
        newdata = perfis, 
        type = 'response')

#-----------------------------------------------------------------------