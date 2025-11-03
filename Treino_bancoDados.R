readLines("Excel_treino.csv", n = 5) # houve problemas de separador. não era nem ponto, nme vírgula
dados <- read.csv("Excel_treino.csv", 
                  header = TRUE, 
                  sep = ";", 
                  skip = 4, 
                  check.names = FALSE) #mantem caracteres especiais como acentos
head(dados)
str(dados)

install.packages("tidyr")   # ou install.packages("tidyverse")
library(tidyr)
library(dplyr)

names(dados)

dados_long <- dados %>%
  pivot_longer(
    cols = -`Munic\xedpio de resid\xeancia`,       # mantém a coluna de município
    names_to = "Ano",                        # nome da nova coluna
    values_to = "Casos"                      # valores numéricos
  )


