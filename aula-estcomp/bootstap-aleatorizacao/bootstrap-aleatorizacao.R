
#----------------------------------------------------------------------
#
# Prof. Lineu Alberto Cavazani de Freitas
# Laboratório de Estatística e Geoinformação
# Departamento de Estatística
# Universidade Federal do Paraná
# URL: https://lineu96.github.io/
#
# COMPARAÇÃO DE DOIS GRUPOS
#
#----------------------------------------------------------------------
# Comparando dois grupos: 

## comprimento da mandíbula (chacal dourado)

## Existe evidência suficiente nos dados que permite afirmar que
## há diferença entre os grupos?

## Dados
male <- c(120.00, 107.00, 110.00, 116.00, 114.00, 
          111.00, 113.00, 117.00, 114.00, 112.00)

female <- c(110.00, 111.00, 107.00, 108.00, 110.00, 
            105.00, 107.00, 106.00, 111.00, 111.00)

## Média e desvio padrão dos grupos
round(c(mean(male), sd(male)), 2)
round(c(mean(female), sd(female)), 2)

## Densidades
plot(density(male), 
     xlim = c(min(c(male, female))-5, max(c(male, female))+5),
     ylim = c(0,max(c(density(male)$y, density(female)$y))),
     col = '#000080', lwd = 2,
     xlab = 'Comprimento da mandíbula',
     ylab = 'Densidade',
     main = 'Comparação entre grupos')

lines(density(female), col = '#8B0000', lwd = 2)

legend(x = "topright", 
       lwd = c(2,2), 
       col= c('#000080','#8B0000'), 
       legend=c("Male",
                "Female"))

abline(v = mean(male), col = '#000080', lwd = 2, lty = 2)
abline(v = mean(female), col = '#8B0000', lwd = 2, lty = 2)

diferenca_observada <- mean(male) - mean(female)
diferenca_observada

#----------------------------------------------------------------------

# Estratégia padrão: teste t
t.test(male, female, var.equal = TRUE)
t.test(male, female)

# Estratégia não paramétrica: teste Wilcoxon/Mann-Whitney
wilcox.test(male, female)

#----------------------------------------------------------------------
# Estratégias alternativas: 
## Bootstrap e teste de aleatorização
#----------------------------------------------------------------------

# Construindo a distribuição amostral via Bootstrap

## 1. Reamostra (com reposição) os dados de cada grupo
## 2. Calcula as médias de cada grupo
## 3. Calcula a diferença das médias
## 4. Repete várias vezes, constroi a distribuição amostral
## 5. Usa a distribuição amostral para fazer inferência

## Gerando as reamostras e calculando a quantidade de interesse
diferencas_bootstrap <- c()

for (i in 1:100000) {
  
  grupo1_bootstrap <- sample(male, size = 10, replace = TRUE)
  grupo2_bootstrap <- sample(female, size = 10, replace = TRUE)
  diferencas_bootstrap[i] <- mean(grupo1_bootstrap) - mean(grupo2_bootstrap)
  
}

## Construindo a distribuição amostral
hist(diferencas_bootstrap,
     col = 0,
     probability = T,
     xlab = 'Diferenças', 
     ylab = 'Densidade',
     main = 'Distribuição amostral construída via bootstrap',
     ylim = c(0, max(density(diferencas_bootstrap)$y) + 0.45*max(density(diferencas_bootstrap)$y)))

lines(density(diferencas_bootstrap), col = 1, lwd = 3)

## Obtendo estimativa pontual e intervalar

abline(v = mean(diferencas_bootstrap), col = 2, lwd = 4)

abline(v = quantile(diferencas_bootstrap, 
                    probs = c(0.025, 0.975)),
       col = 4, lwd = 5)

legend(x = "topright", 
       lwd = c(3,3), 
       col= c(2,4), 
       legend=c("Estimativa pontual",
                "Estimativa intervalar"))

mean(diferencas_bootstrap)

quantile(diferencas_bootstrap, 
         probs = c(0.025, 0.975))

#----------------------------------------------------------------------

# Comparando a distribuição bootstrap obtida com a distribuição
# centrada em 0

bootstrap_0 <- diferencas_bootstrap - mean(diferencas_bootstrap)

# Comparando as distribuições gerais
plot(density(diferencas_bootstrap), 
     xlim = c(min(c(bootstrap_0, diferencas_bootstrap)),
              max(c(bootstrap_0, diferencas_bootstrap))),
     ylim = c(0,max(c(density(diferencas_bootstrap)$y, density(bootstrap_0)$y))),
     col = '#000000', lwd = 2,
     xlab = 'Diferenças',
     ylab = 'Densidade',
     main = 'Comparação entre distribuições')

lines(density(bootstrap_0), col = '#C71585', lwd = 2)

legend(x = "topleft", 
       lwd = c(2,2), 
       col= c('#000000','#C71585'), 
       legend=c("Distribuição bootstrap",
                "Distribuição bootstrap centrada em 0"))

abline(v = mean(diferencas_bootstrap), col = '#000000', lwd = 2, lty = 2)
abline(v = mean(bootstrap_0), col = '#C71585', lwd = 2, lty = 2)

## Calculando o p-valor
2*(sum(bootstrap_0 >= diferenca_observada)/length(bootstrap_0))

#----------------------------------------------------------------------

# Testando hipóteses via teste de aleatorização

## 1. Colocar os elementos dos grupos num único grupo
## 2. Sorteia 10, aloca a um grupo
## 3. Aloca os 10 restantes no outro grupo
## 4. Calcula as médias de cada grupo
## 5. Calcula a diferença das médias
## 6. Repete várias vezes, constroi a distribuição sob H0
##    e verifica onde o valor observado está nesta distribuição

# Hipóteses do teste

## H0: as médias dos grupos são iguais, ou seja,
##     a diferença entre as médias é igual a 0
       
## H1: as médias dos grupos são diferentes, ou seja,
##     a diferença entre as médias é diferente de 0

#----------------------------------------------------------------------

## Número de combinações possíveis de 20 tomados 10 a 10
choose(n = 20, k = 10)

## Criando um vetor com os 20 elementos
grupo_geral <- c(male, female)

## Gerando as diferenças via aleatorização
diferencas_aleatorizacao <- c()

for (i in 1:100000) {
  indices <- 1:20
  sorteio <- sample(indices, size = 20, replace = FALSE)
  
  grupo1_aleatorizado <- grupo_geral[sorteio[1:10]]
  grupo2_aleatorizado <- grupo_geral[sorteio[11:20]]
  
  diferencas_aleatorizacao[i] <- mean(grupo1_aleatorizado) - mean(grupo2_aleatorizado) 
}

## Distribuição amostral sob H0
hist(diferencas_aleatorizacao,
     col = 0,
     probability = T,
     xlab = 'Diferenças', 
     ylab = 'Densidade',
     main = 'Distribuição sob H0 construída por aleatorização',
     ylim = c(0, max(density(diferencas_aleatorizacao)$y) + 0.45*max(density(diferencas_aleatorizacao)$y)))

lines(density(diferencas_aleatorizacao), col = 1, lwd = 3)

## Confrontando com o valor observado
abline(v = diferenca_observada, col = 2, lwd = 4)

## Calculando o p-valor
2*(sum(diferencas_aleatorizacao >= diferenca_observada)/length(diferencas_aleatorizacao))

#----------------------------------------------------------------------

# Comparando as distribuições gerais
plot(density(diferencas_bootstrap), 
     xlim = c(min(c(diferencas_aleatorizacao, diferencas_bootstrap)),
              max(c(diferencas_aleatorizacao, diferencas_bootstrap))),
     ylim = c(0,max(c(density(diferencas_bootstrap)$y, density(diferencas_aleatorizacao)$y))),
     col = '#006400', lwd = 2,
     xlab = 'Diferenças',
     ylab = 'Densidade',
     main = 'Comparação entre distribuições')

lines(density(bootstrap_0), col = '#4B0082', lwd = 2)
lines(density(diferencas_aleatorizacao), col = '#FF4500', lwd = 2)


legend(x = "topleft", 
       lwd = c(2,2,2), 
       col= c('#000080','#363636','#8B0000'), 
       legend=c("Distribuição amostral (bootstrap)",
                "Distribuição bootstrap centrada em 0",
                "Distribuição sob H0 (aleatorização)"))

#----------------------------------------------------------------------

# Exercício sugerido

## Testes de hipóteses para duas variâncias são comuns 
## em situações em que temos interesse em vefificar se 
## a variabilidade entre dois grupos é similar. 

## Os testes clássicos para comparação de variâncias tem 
## como requisito a normalidade dos grupos, o que na 
## prática costuma ser difícil de observar. 

## Neste contexto, testes computacionalmente intensivos 
## podem ser uma alternativa, pois são livres de suposições 
## distribucionais.

## Suponha que temos dois métodos de produção de 
## determinado componente, contudo, um deles é mais barato. 

## Para que o método mais barato possa ser colocado em 
## produção é necessário que a precisão dos componentes 
## produzidos sejam similares entre os métodos, para que 
## não haja perda de qualidade. 

## Se utilizarmos a variância como uma estimativa de 
## precisão, uma possibilidade é avaliar a razão entre as 
## variâncias dos grupos. 

## Se houver evidência suficiente nos dados que permita 
## afirmar que a razão é igual a 1, podemos concluir que 
## a variabilidade entre os métodos é similar.

## Considere os dados e avalie se os métodos são ou não
## similares usando os métodos discutidos.

metodo1 <- 
  c(5.7, 3.4, 6.1, 5.9, 11.3, 
  11.9, 19.3, 14.4, 17.6, 8.7, 
  12.3, 9.1, 14.9, 15.8, 4.1, 
  10.4, 15.6, 1.0, 13.1, 7.7)

metodo2 <- 
  c(8.4, 6.9, 16.2, 9.5, 10.6,
    3.2, 6.8, 5.9, 11.8, 17.4,
    6.9, 13.8, 3.2, 9.6, 3.3,
    9.2, 19.8, 10.6, 14.0, 14.6)

#----------------------------------------------------------------------
