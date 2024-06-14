#----------------------------------------------------------------------
#
# MÉTODOS ESTATÍSTICOS EM PESQUISA CIENTÍFICA - MEPC
#
# Modelos de Regressão para Dados de Contagem
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

## Os dados extraídos da base de dados pública do Ipardes e 
## dizem respeito ao número de acidentes de trânsito em 
## municípios do Paraná no ano de 2010. 

## O Batalhão de Polícia de Trânsito - BPTRAN define como 
## acidente de trânsito todo evento ocorrido na via pública, 
## inclusive calçadas, decorrente do trânsito de veículos e 
## pessoas, que resulta em danos humanos e materiais.

## Compreende: colisões entre veículos, choque com objetos 
## fixos, capotamentos, tombamentos, atropelamentos, 
## queda de pedestres e ciclistas, etc. 

## Além disso toda ocorrência fortuita ou danosa, 
## envolvendo veículos em circulação, ou parados, 
## respectivos ocupantes, pedestres e objetos móveis ou fixos.

## Os dados consideram apenas os acidentes de trânsito ocorridos 
## nas vias municipais (não foram incluídos acidentes ocorridos 
## nas Rodovias Estaduais e Federais).

## Cada linha da base diz respeito a 1 dos 399 municípios do 
## estado do Paraná, foram coletadas as variáveis:

#----------------------------------------------------------------------

# Variáveis

## actt - Número de acidentes de trânsito no município
## pibpc - Produto Interno Bruto per Capita do município.
## ater - Área Territorial (km²).
## gurb - Grau de Urbanização, percentagem da população da área urbana em relação à população total.
## frvei - Frota total de veículos.
## pop - População Censitária.

#----------------------------------------------------------------------

# O objetivo da análise foi modelar o número de acidentes de 
# trânsito em função das demais variáveis.

ipardes  <- read.csv2('https://raw.githubusercontent.com/lineu96/glm/master/consulta.csv', 
                      header = T, sep = ';', dec = ',')

View(ipardes)
head(ipardes)

#----------------------------------------------------------------------

# Análise exploratória

library(psych)

pairs.panels(ipardes[ , 2:7], 
             method = "pearson",
             hist.col = 2,
             density = TRUE, 
             ellipses = FALSE,
             pch = 20,
             lwd = 0.01
)


## Transformando as variáveis explicativas
ipardes$lpibpc <- log(ipardes$pibpc)
ipardes$later  <- log(ipardes$ater)
ipardes$lgurb <- log(ipardes$gurb)
ipardes$lfrvei <- log(ipardes$frvei)
ipardes$lpop   <- log(ipardes$pop)

pairs.panels(ipardes[ , c(2,8:12)], 
             method = "pearson",
             hist.col = 2,
             density = TRUE, 
             ellipses = FALSE,
             pch = 20,
             lwd = 0.01
)

#----------------------------------------------------------------------

# Ajuste dos modelos

## Vamos explorar 4 modelos e verificar qual deles perfoma melhor
## Consideraremos as distribuições Poisson e Binomial Negativa
## Em uma estratégia usaremos as covariáveis originais
## Em outra estratégia as covariáveis transformadas

library(MASS)

pois <- glm(actt ~ pibpc + ater + gurb + frvei + pop, 
            data = ipardes, family = 'poisson')

bin_neg <- glm.nb(actt ~ pibpc + ater + gurb + frvei + pop, 
                  data = ipardes)

pois2 <- glm(actt ~ lpibpc + later + lgurb + lfrvei + lpop, 
             data = ipardes, family = 'poisson')

bin_neg2 <- glm.nb(actt ~ lpibpc + later + lgurb + lfrvei + lpop, 
                   data = ipardes)

#----------------------------------------------------------------------

# Comparando ajustes

ajuste <- c('Poisson', 'Binomial Negativa',
           'Poisson 2', 'Binomial Negativa 2')

aic <- c(AIC(pois), 
         AIC(bin_neg),
         AIC(pois2), 
         AIC(bin_neg2))

verossimilhanca <- c(logLik(pois), 
                     logLik(bin_neg),
                     logLik(pois2), 
                     logLik(bin_neg2))

data.frame(ajuste, aic, verossimilhanca)

#----------------------------------------------------------------------

# Resumo do modelo selecionado
summary(bin_neg2)

#----------------------------------------------------------------------

# Reajustando o modelo
bin_neg2_step <- step(bin_neg2, direction = "both")
summary(bin_neg2_step)

#----------------------------------------------------------------------

# Comparando modelos
anova(bin_neg2, bin_neg2_step, test = 'Chisq')

#----------------------------------------------------------------------

# Análise dos resíduos quantílicos aleatorizados

library(statmod)
par(mfrow=c(1,2))
res <- qresiduals(bin_neg2_step)
plot(res)
residuos <- qresiduals(bin_neg2_step)
qqnorm(residuos)
qqline(residuos, col = 2)

#----------------------------------------------------------------------

# Predição

perfis <- data.frame(lpibpc =  log(c(6500, 13000, 16000)),
                     lgurb =  log(c(10, 75, 84)), 
                     lfrvei =  log(c(400,  3500,  7000)),
                     lpop =  log(c(1500, 9000, 17000)))

predict(bin_neg2_step, 
        interval = 'prediction', 
        newdata = perfis, 
        type = 'response')

#----------------------------------------------------------------------
