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
# AULA 5 - Distribuições de probabilidades no R
#
#-----------------------------------------------------------------------

# O R possui funcionalidades para operações com distribuições de
# probabilidades. 

# Para cada distribuição há 4 operações básicas indicadas pelas letras:

# d: calcula a fp ou fdp em um ponto.
# p: calcula a função de probabilidade acumulada até um ponto.
# q: calcula o quantil correspondente a uma dada probabilidade;
# r: gera uma amostra aleatória da distribuição.

#-----------------------------------------------------------------------

# Bernoulli

## Variável aleatória assume apenas os valores 0 (“fracasso”) ou 1 
## (“sucesso”).

## Considere o lançamento de uma moeda em que a probabilidade de
## cara é 0,7 e a probabilidade de coroa é 0,3. Considere que "cara"
## representa um sucesso e "coroa" um fracasso.

caras <- 0:1

prob <- dbinom(caras, size = 1, prob = 0.7)

prob_ac <- pbinom(caras, size = 1, prob = 0.7)

ex_bernoulli <- data.frame(caras,
                           prob,
                           prob_ac)

ex_bernoulli

sum(ex_bernoulli$prob)

plot(prob~caras, ex_bernoulli, 
     type = 'h', ylim = c(0,1))

points(x = ex_bernoulli$caras,
       y = ex_bernoulli$prob,
       pch = 19)

#-----------------------------------------------------------------------

# Binomial

## Experimento aleatório que consiste de mais de uma tentativa
## de Bernoulli. 

## Cada tentativa é o desfecho de uma variável dicotômica.

## As tentativas devem ser independentes. 

## A probabilidade de sucesso em cada tentativa é constante.

## A variável aleatória representa o número de sucessos em n ensaios de 
## Bernoulli

## Os parâmetros são o número de tentativas (n) e a probabilidade de 
## sucesso (p).

## Suponha o experimento de lançar a moeda viciada do exemplo
## anterior 10 vezes.

caras <- 0:10

prob <- dbinom(caras, size = 10, prob = 0.7)

prob_ac <- pbinom(caras, size = 10, prob = 0.7)

ex_binom <- data.frame(caras,
                       prob,
                       prob_ac)

ex_binom

sum(ex_binom$prob)

plot(prob~caras, ex_binom, 
     type = 'h', ylim = c(0,0.3))

points(x = ex_binom$caras, 
       y = ex_binom$prob,
       pch = 19)

# P(X = 1)
dbinom(x = 1, size = 10, prob = 0.7)

# P(X = 3)
dbinom(x = 3, size = 10, prob = 0.7)

#P(6 <= X <= 8) = P(X = 6) + P(X = 7) + P(X = 8)
dbinom(x = 6, size = 10, prob = 0.7) +
  dbinom(x = 7, size = 10, prob = 0.7) +
  dbinom(x = 8, size = 10, prob = 0.7)

#-----------------------------------------------------------------------

# Exemplo Poisson

## Distribuição usada para modelar problemas de contagens.

## Número de eventos em um domínio (como tempo e espaço).

## Taxa de ocorrência constante (probabilidade de um evento é a
## mesma para qualquer unidade de mesma dimensão).

## Independência entre domínios disjuntos.

##  Taxa proporcional ao tamanho do domínio.

## A variável aleatória representa o número de ocorrências em
## um intervalo.

## Suponha que o número de requisições feitas a determinado site do
## governo se comporta segundo uma distribuição de Poisson com taxa
## de 5 requisições por minuto.

contagens <- 0:15

prob <- round(dpois(contagens, lambda = 5), 3)

prob_ac <- round(ppois(contagens, lambda = 5), 3)

ex_poisson <- data.frame(contagens,
                         prob,
                         prob_ac)

ex_poisson

sum(ex_poisson$prob)

plot(prob~contagens, ex_poisson, 
     type = 'h', ylim = c(0,0.2))

points(x = ex_poisson$contagens, 
       y = ex_poisson$prob,
       pch = 19)

# Qual a probabilidade de não haver 
# requisições em um minuto?
dpois(x = 0, lambda = 5)

# Qual a probabilidade de haver 10 
# requisições em um minuto?
dpois(x = 10, lambda = 5)

# Qual a probabilidade de haver entre 3 e 6 
# requisições em um minuto?
dpois(x = 3, lambda = 5) + 
  dpois(x = 4, lambda = 5) + 
  dpois(x = 5, lambda = 5) +
  dpois(x = 6, lambda = 5)

ppois(6, lambda = 5) - ppois(2, lambda = 5)

#-----------------------------------------------------------------------

# Exemplo hipergeométrica

## Amostrar sem reposição um número de elementos de um conjunto em 
## que dois resultados possíveis (sucesso ou fracasso).

## Todos os elementos têm igual probabilidade de serem amostrados.

## Conjunto de m + n objetos.
## m > 0 são considerados como sucesso.
## n > 0 são considerados como fracasso.
## Sorteia-se de r objetos r < m + n, ao acaso e sem reposição.
## A variável aleatória é o número de objetos do tipo sucesso.

## Suponha que em um parque estime-se que hajam 200 macacos de
## determinada espécie. Destes macacos, 50 foram capturados,
## marcados e soltos no parque. Se forem amostrados 10 macacos,
## qual a probabilidade de encontrar pelo menos um macaco marcado?

macacos <- 0:10

prob <- dhyper(x = macacos, m = 50, n = 150, 
               k = 10, 
               log = FALSE)

prob_ac <- phyper(macacos, 
                  m = 50, n = 150, k = 10,)

ex_hiper <- data.frame(macacos,
                       prob,
                       prob_ac)

round(ex_hiper, 3)

sum(ex_hiper$prob)

plot(prob~macacos, ex_hiper, 
     type = 'h', ylim = c(0,0.3))

points(x = ex_hiper$macacos, 
       y = ex_hiper$prob,
       pch = 19)

# P(X >= 1)

dhyper(x = 1, m=50, n=150, k=10, log = FALSE) +
  dhyper(x = 2, m=50, n=150, k=10, log = FALSE) +
  dhyper(x = 3, m=50, n=150, k=10, log = FALSE) +
  dhyper(x = 4, m=50, n=150, k=10, log = FALSE) +
  dhyper(x = 5, m=50, n=150, k=10, log = FALSE) +
  dhyper(x = 6, m=50, n=150, k=10, log = FALSE) +
  dhyper(x = 7, m=50, n=150, k=10, log = FALSE) +
  dhyper(x = 8, m=50, n=150, k=10, log = FALSE) +
  dhyper(x = 9, m=50, n=150, k=10, log = FALSE) +
  dhyper(x = 10, m=50, n=150, k=10, log = FALSE)

sum(dhyper(x = 1:10, m=50, n=150, k=10, 
           log = FALSE))

1 - dhyper(x = 0, m=50, n=150, k=10, log = FALSE)

phyper(0, m=50, n=150, k=10, log = FALSE, lower.tail = FALSE)

#-----------------------------------------------------------------------

# Exemplo exponencial

## Usada para modelar variáveis aleatórias contínuas não negativas.

## Muito usada para modelar problemas que dizem respeito ao
## tempo até ocorrência de um evento.

## Tem como característica a falta de memória.
## Propensão à falha independe do tempo decorrido.

## A duração do atendimento de cada cliente pelo sistema drive-thru
## de uma rede fast food tem distribuição Exponencial com tempo
## médio de atendimento de 10 minutos, o que implica em α = 1/10.

plot(0:60, dexp(x = 0:60, rate = 1/10), type = "l",
     xlab = "x", ylab = "f(x)", 
     main = expression(alpha == 1/10),
     ylim = c(0,0.11), axes = TRUE)

#P(X<5)
abline(v = 5)
pexp(q = 5, rate = 1/10)
exp(-1/10 * 0) - exp(-1/10 * 5)

# P(4<X<6)
abline(v = c(4,6))
pexp(q = 6, rate = 1/10) - pexp(q = 4, rate = 1/10)

#-----------------------------------------------------------------------

# Exemplo normal

## Modela variáveis aleatórias contínuas não limitadas.

## Tem comportamento simétrico: média, mediana e moda coincidem.

## A função densidade de probabilidade é complexa.

## A integral da função não tem forma fechada.

## São necessários métodos numéricos ou a consulta a tabelas.

## Considere que altura de indivíduos de determinada modalidade
## esportiva se comporta como um modelo Normal com média 180 cm
## e variância de 49 cm.

plot(seq(160, 200, length=100), type = "l", 
     xlab = "X", ylab = "f(x)",
     y = dnorm(x = seq(160, 200, length=100), 
               mean = 180, sd = sqrt(49)),
     main = expression(list(mu == 180, 
                            sigma^2 == 49)),
     ylim = c(0,0.06))

# P(X > 190)
abline(v = 190)
1-pnorm(q = 190, mean = 180, sd = sqrt(49))

# P(X < 160)
abline(v = 160)
pnorm(q = 160, mean = 180, sd = sqrt(49))

# P(175 < X < 185)
abline(v = c(175, 185))

pnorm(q = 185, mean = 180, sd = sqrt(49)) - 
  pnorm(q = 175, mean = 180, sd = sqrt(49))

#-----------------------------------------------------------------------

# SUGESTÃO 

## Busque replicar os procedimentos com outas distribuições vistas em
## aula: Binomial negativa, Geométrica, Uniforme, Gama, etc.

## Algumas outras distribuições disponíveis no R:

### Beta: pbeta	qbeta	dbeta	rbeta

### Cauchy: pcauchy	qcauchy	dcauchy	rcauchy

### Chi-Square: pchisq	qchisq	dchisq	rchisq

### F: pf	qf	df	rf

### Gamma: pgamma	qgamma	dgamma	rgamma

### Geometric: pgeom	qgeom	dgeom	rgeom

### Logistic: plogis	qlogis	dlogis	rlogis

### Log Normal: plnorm	qlnorm	dlnorm	rlnorm

### Negative Binomial: pnbinom	qnbinom	dnbinom	rnbinom

### Student t: pt	qt	dt	rt

### Uniform: punif	qunif	dunif	runif

### Weibull: pweibull	qweibull	dweibull	rweibull

#-----------------------------------------------------------------------

# CRAN Task View: Probability Distributions

# https://cran.r-project.org/web/views/Distributions.html

#-----------------------------------------------------------------------