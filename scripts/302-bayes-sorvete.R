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
# AULA 4 - R como calculadora em uma aplicação do teorema de bayes
#-----------------------------------------------------------------------

# Suponha que um fabricante de sorvetes recebe 20% de todo o leite 
# que utiliza de uma fazenda F1, 30% de uma outra fazenda F2 e
# 50% de F3.

# Um órgão de fiscalização inspecionou as fazendas de surpresa e 
# observou que 20% do leite produzido por F1 estava adulterado por
# adição de água, enquanto para F2 e F3, essa proporção era de 
# 5% e 2%, respectivamente.

# Na indústria de sorvetes os galões de leite são armazenados em um 
# refrigerador sem identificação das fazendas. 

# Para um galão escolhido ao acaso, qual a probabilidade do
# leite estar adulterado?

#-----------------------------------------------------------------------

#F1: leite da fazenda 1
#F2: leite da fazenda 2
#F3: leite da fazenda 3
#A: leite adulterado

#-----------------------------------------------------------------------

# P(F1) = 0,20
# P(F2) = 0,30
# P(F3) = 0,50

PF1 <- 0.2
PF2 <- 0.3
PF3 <- 0.5

# P(A|F1) = 0,20
# P(A|F2) = 0,05
# P(A|F3) = 0,02

PAdadoF1 <- 0.2
PAdadoF2 <- 0.05
PAdadoF3 <- 0.02

#-----------------------------------------------------------------------

# P(A) = P(A|F1)P(F1) + P(A|F2)P(F2) + P(A|F3)P(F3)

c(PF1, PF2, PF3)
c(PAdadoF1, PAdadoF2, PAdadoF3)

c(PF1, PF2, PF3) * c(PAdadoF1, PAdadoF2, PAdadoF3)

sum(c(PF1, PF2, PF3) * c(PAdadoF1, PAdadoF2, PAdadoF3))

PA <- sum(c(PF1, PF2, PF3) * c(PAdadoF1, PAdadoF2, PAdadoF3))

#-----------------------------------------------------------------------

PF1dadoA <- (PAdadoF1*PF1)/PA
PF1dadoA

PF2dadoA <- (PAdadoF2*PF2)/PA
PF2dadoA

PF3dadoA <- (PAdadoF3*PF3)/PA
PF3dadoA

#-----------------------------------------------------------------------