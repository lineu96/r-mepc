
#-----------------------------------------------------------------------
#
# MÉTODOS ESTATÍSTICOS EM PESQUISA CIENTÍFICA - MEPC
# OFERTA 2023
# APOIO COMPUTACIONAL EM LINGUAGEM R
#
#-----------------------------------------------------------------------
# 
# Prof. Lineu Alberto Cavazani de Freitas
# Laboratório de Estatística e Geoinformação
# Departamento de Estatística
# Universidade Federal do Paraná
# URL: https://lineu96.github.io/
#
# AULA 6 - Ilustração distribuição amostral da média
#
#-----------------------------------------------------------------------

## Definindo alguns parâmetros
tamanho_populacao <- 1000
tamanho_amostra <- 100
n_amostras <- 500

## Gerando uma população (pode ser qualquer distribuição)
# populacao <- rnorm(tamanho_populacao, 
#                    mean = 170, 
#                    sd = 10)

populacao <- rpois(tamanho_populacao, 
                    lambda = 170)

populacao <- runif(tamanho_populacao)

 populacao <- rbinom(tamanho_populacao,
                     size = 1,
                     prob = 0.65)


## Coletando diferentes amostras
amostras <- matrix(nrow = tamanho_amostra, 
                   ncol = n_amostras)

for(i in 1:n_amostras) {
  amostras[,i] <- sample(populacao, 
                         size = tamanho_amostra, 
                         replace = TRUE)  
}

amostras <- as.data.frame(amostras)
dim(amostras)

## Calculando a média de cada amostra
medias <- colMeans(amostras)

## Uma estimativa pontual baseada na distribuição amostral
mean(medias)

## Uma estimativa intervalar baseada na distribuição amostral
quantile(medias, probs = c(0.025, 0.975))

## Distribuição amostral de forma gráfica
hist(medias,
     col = 0,
     probability = T,
     xlab = 'Médias', 
     ylab = 'Densidade',
     main = 'Distribuição amostral da média')

lines(density(medias), col = 4, lwd = 5)

abline(v = quantile(medias, 
                    probs = c(0.025, 0.975)),
       col = 2, lwd = 5)

abline(v = mean(medias), col = 2, lwd = 3)

abline(v = mean(populacao), col = 3, lwd = 3)

mean(populacao)

#-----------------------------------------------------------------------
