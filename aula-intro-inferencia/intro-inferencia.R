
#----------------------------------------------------------------------
#
# Prof. Lineu Alberto Cavazani de Freitas
# Laboratório de Estatística e Geoinformação
# Departamento de Estatística
# Universidade Federal do Paraná
# URL: https://lineu96.github.io/
#
# INTRODUÇÃO A INFERÊNCIA
# DISTRIBUIÇÃO AMOSTRAL DA MÉDIA
#
#----------------------------------------------------------------------

## População

#populacao <- rnorm(30, 170,10)
quest <- read.csv("respostas-2024.csv")
populacao <- quest$Qual.a.sua.altura.
length(populacao)

hist(populacao, 
     xlab = 'Altura', 
     ylab = 'Frequência',
     main = 'Histograma da população')

#-----------------------------------------------------------------------

## Definindo algumas quantidades
tamanho_amostra <- 20
n_amostras <- 100

#-----------------------------------------------------------------------

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
View(amostras)
#-----------------------------------------------------------------------

## Calculando a média de cada amostra
medias <- colMeans(amostras)
#View(medias)
#-----------------------------------------------------------------------

## Distribuição amostral empírica de forma gráfica

hist(medias,
     col = 0,
     probability = T,
     xlab = 'Médias', 
     ylab = 'Densidade',
     main = 'Distribuição amostral da média',
     ylim = c(0, max(density(medias)$y) + 0.1*max(density(medias)$y)))

lines(density(medias), col = 1, lwd = 3)

legend(x = "topright", 
       lwd = c(3,3,3), 
       col= c(2,3,4), 
       legend=c("Mediana",
                "Média",
                "Quantis 5% e 95%"))

#-----------------------------------------------------------------------

## Adicionando quantidades no gráfico
### Mediana
abline(v = quantile(medias, 
                    probs = 0.5),
       col = 2, lwd = 5)

### Média
abline(v = mean(medias),
       col = 3, lwd = 5)

### Quantis
abline(v = quantile(medias, 
                    probs = c(0.05, 0.95)),
       col = 4, lwd = 5)

### Valor real do parâmetro
abline(v = mean(populacao), col = 1, lwd = 5)

### Comparando os valores
t(data.frame(media = mean(medias),
             mediana = median(medias),
             verdadeiro = mean(populacao)))

## Uma estimativa intervalar baseada na distribuição amostral
quantile(medias, probs = c(0.05, 0.95))

#-----------------------------------------------------------------------

## Centrando a distribuição amostral na média de uma amostra
i=1
contem <- c()

for (i in 1:100) {
  
  Sys.sleep(0.4)
  
  ### Média e desvio padrão
  media_amostra <- mean(amostras[,i])
  dp_media_amostra <- sd(populacao)/sqrt(nrow(amostras))
  
  ### Gráfico
  inferior <- media_amostra - dp_media_amostra
  superior <- media_amostra + dp_media_amostra
  
  x <- seq(-4, 4, 
           length = 1000) * dp_media_amostra + media_amostra
  
  y <- dnorm(x, media_amostra, dp_media_amostra)
  
  plot(x,y, 
       type = "l", 
       lwd = 2, 
       axes = FALSE, 
       xlab = "", 
       ylab = "",
       main = paste0(i))
  
  pontos_x <- 
    seq(-5 * dp_media_amostra + media_amostra,
        5 * dp_media_amostra + media_amostra,
        by = dp_media_amostra)
  
  axis(side = 1, at = round(pontos_x, 1), pos = 0)
  
  ## Adicionando quantidades no gráfico
  
  ### Mediana
  abline(v = qnorm(0.5, media_amostra, 
                   dp_media_amostra),
         col = 2, lwd = 5)
  
  ### Média amostral
  abline(v = media_amostra,
         col = 3, lwd = 5)
  
  ### Quantis
  abline(v = qnorm(c(0.05, 0.95),
                   media_amostra, 
                   dp_media_amostra),
         col = 2, lwd = 5)
  
  ## Verdadeira média
  abline(v = mean(populacao), col = 1, lwd = 5)
  
  
  ## Contando intervalos que contém a verdadeira média
  contem[i] <- 
    mean(populacao) >= qnorm(0.05,
                             media_amostra, 
                             dp_media_amostra) &
    
    mean(populacao) <= qnorm(0.95,
                             media_amostra, 
                             dp_media_amostra)
  
}

(sum(contem)/length(contem))*100

#-----------------------------------------------------------------------

# Plotando o valor do parâmetro e todas as distribuições
# amostrais estimadas

plot(0, type = 'n', 
     xlim = c(min(medias) - 4*sd(medias),
              max(medias) + 4*sd(medias)),
     ylim = c(0, (max(density(medias)$y) + 0.1*max(density(medias)$y))),
     xlab = '',
     ylab = '')

## Centrando a distribuição amostral na média de uma amostra
i=1
for (i in 1:100) {
  
  Sys.sleep(0.3)
  
  ### Média e desvio padrão
  media_amostra <- mean(amostras[,i])
  dp_amostra <- sd(populacao)/sqrt(nrow(amostras))
  
  ### Gráfico
  inferior <- media_amostra - dp_amostra
  superior <- media_amostra + dp_amostra
  
  x <- seq(-4, 4, 
           length = 1000) * dp_amostra + media_amostra
  
  y <- dnorm(x, media_amostra, dp_amostra)
  
  lines(x,y, 
        type = "l", 
        lwd = 2,
        col = i)
  
  ## Verdadeira média
  abline(v = mean(populacao), col = 1, lwd = 7)
  
}

#-----------------------------------------------------------------------

### Média e desvio padrão VERDADEIROS
media_pop <- mean(populacao)
dp_media_pop <- sd(populacao)/sqrt(nrow(amostras))

### Gráfico
inferior <- media_pop - dp_media_pop
superior <- media_pop + dp_media_pop

x <- seq(-4, 4, 
         length = 1000) * dp_media_pop + media_pop

y <- dnorm(x, media_pop, dp_media_pop)

lines(x,y, 
      type = "l", 
      lwd = 10,
      col = 1)

lines(density(medias), col = 2	, lwd = 10)

## Verdadeira média
abline(v = mean(populacao), col = 1, lwd = 10)

#-----------------------------------------------------------------------

# Plotando apenas a distribuição amostral empírica e a 
# baseada no teorema do limite central

plot(0, type = 'n', 
     xlim = c(min(medias) - 4*sd(medias),
              max(medias) + 4*sd(medias)),
     ylim = c(0, (max(density(medias)$y) + 0.15*max(density(medias)$y))),
     xlab = '',
     ylab = '')

### Gráfico
inferior <- media_pop - dp_media_pop
superior <- media_pop + dp_media_pop

x <- seq(-4, 4, 
         length = 1000) * dp_media_pop + media_pop

y <- dnorm(x, media_pop, dp_media_pop)

lines(x,y, 
      type = "l", 
      lwd = 10,
      col = 1)

lines(density(medias), col = 2	, lwd = 10)

#-----------------------------------------------------------------------