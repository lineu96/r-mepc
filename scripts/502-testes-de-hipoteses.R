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
# AULA 6 - Alguns testes de hipóteses implementados em R
#
#-----------------------------------------------------------------------

# Teste de hipótese para uma média

## Teste t

## Usado para comparar a média de uma amostra com um valor
## conhecido (teórico/hipotética).

## Pode ser usado apenas quando os dados são normalmente 
## distribuídos.

altura <- rnorm(100, 170, 10)

t.test(altura, 
       mu = 170, 
       alternative = "two.sided")

#-----------------------------------------------------------------------

# Teste de hipótese para uma proporção.

## Teste z

## Usado para comparar a proporção de elementos pertencentes
## a uma categoria com um valor conhecido 
## (teórico/hipotético).

486/1000
prop.test(x = 486, n = 1000, p = 0.5)

#-----------------------------------------------------------------------

# Teste de hipótese para diferença de duas médias 
# (amostras não pareadas)

## Teste t

## Usado para comparar a média de dois grupos independentes. 

## Os grupos devem ser normalmente distribuídos e com mesma 
## variância

altura_homens <- rnorm(100, mean = 160, 10)
altura_mulheres <- rnorm(100, mean = 175, 10)

t.test(altura_homens, 
       altura_mulheres, 
       alternative = "two.sided")

# Se as variâncias dos dois grupos comparados forem 
## diferentes é possível usar o teste t de Welch.

x <- rnorm(100, mean = 160, 5)
y <- rnorm(100, mean = 175, 15)

t.test(x, y, 
       alternative = "two.sided", 
       var.equal = FALSE)

#-----------------------------------------------------------------------

# Teste de hipótese para diferença de duas médias 
# (amostras pareadas)

## Usado para comparar as médias entre dois grupos 
## relacionados. 
## Para cada observação existe um par de valores

## O vetor de diferenças deve seguir distribuição normal

x <- rnorm(100, 5, 2)
y <- rnorm(100, 8, 2)
t.test(x, y, paired = TRUE, alternative = "two.sided")

#-----------------------------------------------------------------------

# Teste de hipótese para diferença de duas proporções.

## Teste z

## Usado para comparar duas proporções observadas.

prop.test(x = c(300, 350), 
          n = c(500, 500))

#-----------------------------------------------------------------------

# Teste de hipótese para duas variâncias.

## Teste F

## Usado para avaliar se as variâncias de duas populações
## são iguais.

## Requer que as duas populações sigam distribuição Normal

x <- rnorm(100, 100, 50)
y <- rnorm(100, 10, 10)

var.test(x, y, 
         alternative = "two.sided")

#-----------------------------------------------------------------------

# Testes qui-quadrado para associação

## Utilizado para analisar tabelas de contingência formada 
## por duas variáveis categóricas. 

## Avalia se existe associação significativa entre as 
## categorias das duas variáveis. 

x <- sample(letters[1:3], size = 500, replace = TRUE)
y <- sample(letters[4:6], size = 500, replace = TRUE)
tabela <- table(x,y)

chisq.test(tabela)

#-----------------------------------------------------------------------

# Teste para correlação

## Usado para avaliar a associação entre duas ou mais 
## variáveis.

x <- rnorm(100)
y <- rnorm(100)
cor(x, y, method = c("pearson"))

cor.test(x, y, method=c("pearson"))

#-----------------------------------------------------------------------

# Normalidade

## Usado para avaliar se um conjunto de valores segue ou não 
## distribuição Normal

x <- rnorm(100)
shapiro.test(x)

#-----------------------------------------------------------------------
