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
# AULA 4 - R como calculadora em um problema de indepenência
#-----------------------------------------------------------------------

# Considere o lançamento de um dado honesto e os seguintes eventos: 

## A: face par.
## B: face menor ou igual a 4. 

# Os eventos A e B são independentes?
  
#-----------------------------------------------------------------------

# A: 2, 4, 6
# B: 1, 2, 3, 4
# A inter B: 2, 4

# P(A)
PA <- 3/6

# P(B)
PB <- 4/6

# P(A inter B)
PAiB <- 2/6

#-----------------------------------------------------------------------

## P(A) = P(A|B) e P(B) = P(B|A)
PA == PAiB/PB
PB == PAiB/PA

#-----------------------------------------------------------------------

## P(A ∩ B) = P(A) × P(B).
PAiB == (PA * PB)

#-----------------------------------------------------------------------