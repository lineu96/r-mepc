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

# populacao <- rnorm(30, 170,10)
quest <- read.csv("respostas-2024.csv")
populacao <- quest$Qual.o.seu.peso.
populacao <- populacao[(populacao > 0 & populacao <= 200)] 
length(populacao)

hist(populacao, 
     xlab = 'Altura', 
     ylab = 'Frequência',
     main = 'Histograma da população')

#-----------------------------------------------------------------------

## Definindo algumas quantidades
n_amostras <- 100
tamanho_amostra <- 20

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
#View(amostras)
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
     ylim = c(0, max(density(medias)$y) + 0.2*max(density(medias)$y)))

lines(density(medias), col = 1, lwd = 3)

#-----------------------------------------------------------------------

## Adicionando quantidades no gráfico
media <- mean(medias)
mediana <- median(medias)
ic <- quantile(medias, c(0.05, 0.95))

### Mediana
abline(v = mediana,
       col = 2, lwd = 5)

### Média
abline(v = media,
       col = 3, lwd = 5)

### Quantis
abline(v = ic,
       col = 4, lwd = 5)

### Valor real do parâmetro
abline(v = mean(populacao), col = 1, lwd = 5)

legend(x = "topright", 
       lwd = c(3,3,3,3), 
       col= c(2,3,4,1), 
       legend=c("Mediana",
                "Média",
                "Quantis 5% e 95%",
                "Valor verdadeiro"))

### Comparando os valores
t(data.frame(media = media,
             mediana = mediana,
             verdadeiro = mean(populacao)))

## Uma estimativa intervalar baseada na distribuição amostral
ic

#-----------------------------------------------------------------------

## Construindo os intervalos baseado nas amostras
### Eixo x variando

contem <- c()

for (i in 1:100) {
  
  Sys.sleep(0.3)
  
  ### Média e desvio padrão (populacional)
  media_amostra <- mean(amostras[,i])
  dp_populacao <- sd(populacao)/sqrt(nrow(amostras))
  
  ### Gráfico
  inferior <- media_amostra - (3*dp_populacao)
  superior <- media_amostra + (3*dp_populacao)
  
  ic <- qnorm(c(0.05, 0.95),
              media_amostra, 
              dp_populacao)
  
  plot(media_amostra, 1,
       ylim=range(c(0.5, 1.5)),
       xlim=range(c(inferior, superior)),
       pch=19,
       cex=3,
       xlab="Intervalo de confiança", 
       ylab="",
       yaxt="n",
       main=paste0("Intervalo com 90% confiança para a amostra ", i)
  )
  
  arrows(ic[1], 
         1, 
         ic[2], 
         1, 
         length=0.05, 
         angle=90, 
         code=3,
         lwd = 8)
  
  ## Verdadeira média
  points(x = mean(populacao), y=1, col = 2, pch = 19, cex = 3)
  
  legend(x = "top", 
         pch = c(19,19), 
         col= c(1,2), 
         legend=c("Média amostral",
                  "Média populacional"))
  
  contem[i] <- 
    mean(populacao) >= ic[1] &
    mean(populacao) <= ic[2]
  
  legend(x = "bottom", 
         #pch = c(19,19), 
         col= c(1,2), 
         legend=c(paste0('Contém: ', sum(contem)),
                  paste0('Não contém: ',sum(!contem))))  
}

#-----------------------------------------------------------------------

## Construindo os intervalos baseado nas amostras
### Eixo x fixo

contem <- c()

for (i in 1:100) {
  
  Sys.sleep(0.3)
  
  ### Média e desvio padrão
  media_amostra <- mean(amostras[,i])
  dp_populacao <- sd(populacao)/sqrt(nrow(amostras))
  
  ### Gráfico
  inferior <- mean(sort(populacao)[1:tamanho_amostra])
  superior <- mean(sort(populacao, decreasing = T)[1:tamanho_amostra])
  
  ic <- qnorm(c(0.05, 0.95),
              media_amostra, 
              dp_populacao)
  
  plot(media_amostra, 1,
       ylim=range(c(0.5, 1.5)),
       xlim=range(c(inferior, superior)),
       pch=19,
       cex=3,
       xlab="Intervalo de confiança", 
       ylab="",
       yaxt="n",
       main=paste0("Intervalo com 90% de confiança para a amostra ", i)
  )
  
  abline(v=mean(populacao), lwd = 5, col=2)
  points(x = media_amostra, y=1, col = 1, pch = 19, cex = 3)
  
  arrows(ic[1], 
         1, 
         ic[2], 
         1, 
         length=0.05, 
         angle=90, 
         code=3,
         lwd = 8)
  
  legend(x = "top", 
         pch = c(19,19), 
         col= c(1,2), 
         legend=c("Média amostral",
                  "Média populacional"))
  
  contem[i] <- 
    mean(populacao) >= ic[1] &
    mean(populacao) <= ic[2]
  
  legend(x = "bottom", 
         #pch = c(19,19), 
         col= c(1,2), 
         legend=c(paste0('Contém: ', sum(contem)),
                  paste0('Não contém: ',sum(!contem))))
  
}

#-----------------------------------------------------------------------

## Construindo os intervalos baseado nas amostras
### Todos os intervalos juntos

contem <- c()

dp_populacao <- sd(populacao)/sqrt(nrow(amostras))
inferior <- mean(populacao) - (10*dp_populacao)
superior <- mean(populacao) + (5*dp_populacao)

plot(1:100, 
     rep(mean(populacao),100),
     xlim=c(1,100),
     ylim=c(inferior, superior),
     type = 'l',
     lwd = 5,
     xlab="Intervalo de confiança", 
     ylab="",
     xaxt="n",
     main=paste0("Intervalos com 90% de confiança")
)

for (i in 1:100) {
  
  Sys.sleep(0.3)
  
  ### Média e desvio padrão
  media_amostra <- mean(amostras[,i])
  dp_populacao <- sd(populacao)/sqrt(nrow(amostras))
  
  ic <- qnorm(c(0.05, 0.95),
              media_amostra, 
              dp_populacao)
  
  contem[i] <- 
    mean(populacao) >= ic[1] &
    mean(populacao) <= ic[2]
  
  arrows(i, 
         ic[1], 
         i, 
         ic[2], 
         length=0.05, 
         angle=90, 
         code=3,
         lwd = ifelse(contem[i] == T, 1,3),
         col = ifelse(contem[i] == T, 1,2))
  
  legend(x = "bottom", 
         #pch = c(NA,19), 
         lty = c(1,NA,NA,NA),
         lwd = c(5,NA,NA,NA),
         col= c(1,NA,1,2), 
         legend=c("Média populacional",
                  NA,
                  paste0('Contém: ', sum(contem)),
                  paste0('Não contém: ',sum(!contem))))
  
  
}

#-----------------------------------------------------------------------

# Plotando apenas a distribuição amostral empírica e a 
# baseada no teorema do limite central

plot(0, type = 'n', 
     xlim = c(min(medias) - 4*sd(medias),
              max(medias) + 4*sd(medias)),
     ylim = c(0, (max(density(medias)$y) + 0.15*max(density(medias)$y))),
     xlab = '',
     ylab = '',
     main = 'Comparação distribuição amostral baseada no TLC x distribuição amostral construída')

### Gráfico
inferior <- mean(medias) - dp_populacao
superior <- mean(medias) + dp_populacao

x <- seq(-4, 4, 
         length = 1000) * dp_populacao + mean(medias)

y <- dnorm(x, mean(medias), dp_populacao)

lines(x,y, 
      type = "l", 
      lwd = 10,
      col = 1)

lines(density(medias), col = 2	, lwd = 10)  

legend(x = "topright", 
       #pch = c(NA,19), 
       lty = c(1,1),
       lwd = c(5,5),
       col= c(1,2), 
       legend=c("Dist. amostral TLC",
                "Dis. amostral empírica"))

#-----------------------------------------------------------------------/
