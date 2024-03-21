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
# AULA 3 - Importação/Exportação txt, csv, xlsx
#-----------------------------------------------------------------------

## Apontando o R para o diretório
setwd("CAMINHO")

#----------------------------------------------------------------------

## TXT
dados_txt <- read.table(file = "dados.txt",
                        header = TRUE,
                        sep = ",",
                        dec = ".")

#----------------------------------------------------------------------

## CSV
dados_csv <- read.csv(file = "dados.csv",
                      header = TRUE, 
                      sep = ",", 
                      dec = ".")

#----------------------------------------------------------------------

## EXCEL
#install.packages("openxlsx")
library(openxlsx)

dados_excel <- read.xlsx(xlsxFile = "dados.xlsx",
                         sheet = "dados")

#----------------------------------------------------------------------

## VERIFICANDO SE OS DADOS SÃO IGUAIS
dados_txt == dados_excel
dados_txt == dados_csv
dados_excel == dados_csv

#----------------------------------------------------------------------

## FORMAS ALTERNATIVAS PARA ENTRADA DE DADOS
dados <- edit(data.frame())
dados0

vetor <- scan()
vetor

#----------------------------------------------------------------------

## Exportando dados

write.table(x = dados_txt,
            file = "dados_exportados.txt",
            sep = ",",
            row.names = FALSE,
            col.names = TRUE)

write.table(x = dados_txt,
            file = "dados_exportados.csv",
            sep = ",",
            row.names = FALSE,
            col.names = TRUE)

write.csv(x = dados_txt,
          file = "dados_exportados2.csv")

library(openxlsx)
write.xlsx(x = dados_txt, 
           file = 'dados_exportados.xlsx')

#----------------------------------------------------------------------
