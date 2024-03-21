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
# AULA 1 - Primeiros passos com R
#----------------------------------------------------------------------

# Operações aritméticas básicas

## Soma
1 + 1 

## Subtração
1 - 1 

## Multiplicação
2 * 2 

## Divisão
4/2 

## Potenciação
5^2 

## Radiciação
2^(1/3) 

## Resto
10 %% 3

## Parte inteira
10 %/% 3 

#----------------------------------------------------------------------

# Funções trigonométricas

## Seno
sin(0) 

## Cosseno
cos(0) 

## Tangente
tan(0) 

## Arco seno
asin(0)

## Arco cosseno
acos(1) 

## Arco tangente
atan(0) 

#----------------------------------------------------------------------

# Funções matemáticas especiais

## Exponencial base e
exp(1)

## Raiz quadrada
sqrt(4)	

## Log neperiano
log(10) 

## Log qualquer base
log(10, base = 5) 

## Fatorial
factorial(4)

## Valor absoluto
abs(-1)	

## Arredondamento para cima
ceiling(1.2)

## Arredondamento para baixo
floor(1.2)

## Arredondamento
round(1.2, digits = 0)	

#----------------------------------------------------------------------

## Operadores lógicos

TRUE
FALSE
T
F

## São iguais?
1 == 1 

## São iguais?
1 == 2 

## São diferentes?
2 != 2 

## São diferentes?
1 != 2 

## 2 é menor ou igual a 1?
2 <= 1

## 2 é maior ou igual a 1?
2 >= 1 

## 2 é maior do que 1?
2 > 1 

## 2 é menor do que 1?
2 < 1 

#----------------------------------------------------------------------

# E, OU e NÃO

## 1 é menor que 5 **E** 2 é maior ou igual a 3?
(1 < 5) & (2 >= 3)

## 1 é menor que 5 **OU** 2 é maior ou igual a 3?
(1 < 5) | (2 >= 3)

## 2 é menor que 5? Inverta a resposta lógica.
!(2 < 5)

#----------------------------------------------------------------------

# Valores especiais

NA
NULL
Inf
-Inf
NaN

1+NA
1+NULL
1+Inf
1-Inf
1+NaN
1/0
-1/0
0/0

#----------------------------------------------------------------------

# Variáveis

## Atribuição
x <- 10 # x = 10 também funciona
x

## Operações com variáveis
x+1
x*2
x^2
log(x)
x+x
x*x

## Listando variáveis
ls()

## Removendo variáveis
rm(x)
ls()

#----------------------------------------------------------------------

# Funções de ajuda

## Documentação do pacote base
?base
help(base)

## Buscando pelo termo linear models
help.search("linear models")

## Buscando funções e objetos que tenham plot
apropos("plot")

## Buscando vinhetas associadas ao termo grid
browseVignettes("grid")

## Buscando o termro plot no site do R
RSiteSearch("plot")

#----------------------------------------------------------------------

# Arquivos da linguagem

## Salvando o histórico de comandos executados
savehistory(file = "historico.Rhistory")

## Salvando as variáveis criadas
save.image(file = "variaveis.RData")

#----------------------------------------------------------------------

# Pacotes

## instalando o pacote ggplot2 do CRAN
install.packages("ggplot2")

## carregando o pacote
library(ggplot2)

## Listando o conteúdo do pacote
ls("package:ggplot2")

## Acessando a documentação do pacote
help(package = "ggplot2")

#----------------------------------------------------------------------
