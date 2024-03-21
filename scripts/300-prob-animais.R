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
# AULA 4 - R como calculadora para cálculo de probabilidades

#-----------------------------------------------------------------------
set.seed(1)
#-----------------------------------------------------------------------

sexo <- sample(c("Macho", "Fêmea"), size = 500, replace = T)
especie <- sample(c("A", "B", "C"), size = 500, replace = T)

dados <- data.frame(sexo,
                    especie)

tabela <- addmargins(table(dados))

row.names(tabela)[3] <- "Soma"
colnames(tabela)[4] <- "Soma"

tabela
#-----------------------------------------------------------------------

# 1. Defina os eventos.

## F: ser fêmea
## M: ser macho
## A: ser da espécie A
## B: ser da espécie B
## C: ser da espécie C

#-----------------------------------------------------------------------

# 2. Qual a probabilidade de escolhermos ao acaso uma fêmea?
244/500
PF <- tabela[1,4]/tabela[3,4]
PF

# 3. Qual a probabilidade de escolhermos ao acaso um macho?
tabela[2,4]/tabela[3,4]
PM <- 1 - PF

# 4. Qual a probabilidade de escolhermos ao acaso um animal da 
## espécie A?
PA <- tabela[3,1]/tabela[3,4]
PA

# 5. Qual a probabilidade de escolhermos ao acaso um animal da 
# espécie B?
PB <- tabela[3,2]/tabela[3,4]
PB

# 6. Qual a probabilidade de escolhermos ao acaso um animal da 
# espécie C?

PC <- tabela[3,3]/tabela[3,4]
PC

# 7. Qual a probabilidade de escolhermos ao acaso um macho ou 
# uma fêmea?
PF + PM

#-----------------------------------------------------------------------

# 8. Qual a probabilidade de escolhermos um animal da espécie A ou da 
# espécie B?
PA + PB

# 9. Qual a probabilidade de escolhermos um animal da espécie A ou da 
# espécie C?
PA + PC

# 10. Qual a probabilidade de escolhermos um animal da espécie B ou da 
# espécie C?
PB + PC

# 11. Qual a probabilidade de escolhermos um animal da espécie 
# A ou B ou C?
PA + PB + PC

#-----------------------------------------------------------------------

# 12. Qual a probabilidade de escolhermos uma fêmea ou um animal da 
# espécie A?
PFinterA <- tabela[1,1]/tabela[3,4]
PF + PA - PFinterA

# 13. Qual a probabilidade de escolhermos uma fêmea ou um animal da 
# espécie B?
PFinterB <- tabela[1,2]/tabela[3,4]
PF + PB - PFinterB

# 14. Qual a probabilidade de escolhermos uma fêmea ou um animal da 
# espécie C?
PFinterC <- tabela[1,3]/tabela[3,4]
PF + PC - PFinterC

# 15. Qual a probabilidade de escolhermos um macho ou um animal da 
# espécie A?

# 16. Qual a probabilidade de escolhermos um macho ou um animal da 
# espécie B?

# 17. Qual a probabilidade de escolhermos um macho ou um animal da 
# espécie C?

#-----------------------------------------------------------------------

# 18. Sabendo que é uma fêmea, qual a probabilidade de ser da espécie A?
tabela[1,1]/tabela[1,4]

## P(A|F)
PAdadoF <- PFinterA/PF
PAdadoF

# 19. Sabendo que é uma fêmea, qual a probabilidade de ser da espécie B?

## P(B|F)
PBdadoF <- PFinterB/PF
PBdadoF

# 20. Sabendo que é uma fêmea, qual a probabilidade de ser da espécie C?

## P(C|F)
PCdadoF <- PFinterC/PF
PCdadoF == (1 - (PAdadoF + PBdadoF))

PAdadoF + PBdadoF + PCdadoF

# 21. Sabendo que é um macho, qual a probabilidade de ser da espécie A?
# 22. Sabendo que é um macho, qual a probabilidade de ser da espécie B?
# 23. Sabendo que é um macho, qual a probabilidade de ser da espécie C?

#-----------------------------------------------------------------------

# 24. Sabendo que é da espécie A, qual a probabilidade de ser uma fêmea?

# P(F|A)
PFinterA/PA

# 25. Sabendo que é da espécie B, qual a probabilidade de ser uma fêmea?
# 26. Sabendo que é da espécie C, qual a probabilidade de ser uma fêmea?
# 27. Sabendo que é da espécie A, qual a probabilidade de ser um macho?
# 28. Sabendo que é da espécie B, qual a probabilidade de ser um macho?
# 29. Sabendo que é da espécie C, qual a probabilidade de ser um macho?

#-----------------------------------------------------------------------

# 30. Qual a probabilidade de ser uma fêmea e da espécie A?
74/500
tabela[1,1]/tabela[3,4]
PAdadoF * PF

# 31. Qual a probabilidade de ser uma fêmea e da espécie B?
# 32. Qual a probabilidade de ser uma fêmea e da espécie C?
# 33. Qual a probabilidade de ser uma macho e da espécie A?
# 34. Qual a probabilidade de ser uma macho e da espécie B?
# 35. Qual a probabilidade de ser uma macho e da espécie C?

#-----------------------------------------------------------------------

# 36. O que podemos concluir a respeito da independência das variáveis?

## P(A|B) = P(A) & P(B|A) = P(B)


#-----------------------------------------------------------------------