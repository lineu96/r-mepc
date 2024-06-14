
#----------------------------------------------------------------------
#
# Prof. Lineu Alberto Cavazani de Freitas
# Laboratório de Estatística e Geoinformação
# Departamento de Estatística
# Universidade Federal do Paraná
# URL: https://lineu96.github.io/
#
# BOOSTRAP E TESTE DE ALEATORIZAÇÃO PARA COMPARAÇÃO DE DOIS GRUPOS
#
#----------------------------------------------------------------------
# Comparando dois grupos: comprimento da mandíbula (chacal dourado)

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
     col = '#000080', lwd = 2,
     xlab = 'Diferenças',
     ylab = 'Densidade',
     main = 'Comparação entre distribuições')

lines(density(diferencas_aleatorizacao), col = '#8B0000', lwd = 2)

legend(x = "topleft", 
       lwd = c(2,2), 
       col= c('#000080','#8B0000'), 
       legend=c("Distribuição amostral (bootstrap)",
                "Distribuição sob H0 (aleatorização)"))

#----------------------------------------------------------------------
