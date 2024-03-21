
#-----------------------------------------------------------------------
# Leitura dos dados pela URL

dados_url <- read.csv("https://raw.githubusercontent.com/lineu96/dados/master/dados.csv", 
                  sep = ',', 
                  encoding = 'UTF-8', 
                  header = T)

#-----------------------------------------------------------------------

# Leitura dos dados em formato .txt
dados_txt <- read.table("dados.txt", sep = ',', dec = '.')

#-----------------------------------------------------------------------

# Leitura dos dados em formato .csv
dados_csv <- read.csv("dados.csv")

#-----------------------------------------------------------------------

# Leitura dos dados em formato .xlsx
library("readxl")
dados_xlsx <- read_excel("dados.xlsx")

#-----------------------------------------------------------------------
