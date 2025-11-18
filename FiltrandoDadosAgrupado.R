# ============================================================
# SCRIPT: Limpeza e organização dos dados de municípios
# OBJETIVO: Ler, corrigir e estruturar os dados de casos agrupados por município
# AUTOR: [Seu nome ou grupo]
# DATA: [coloque a data da última modificação]
# ============================================================

# ---------------------------
# 1. Carregar pacotes
# ---------------------------
library(tidyverse)

# ---------------------------
# 2. Importar o arquivo CSV
# ---------------------------

dados <- read_csv2("Dados_agrupados.csv", locale = locale(encoding = "Latin1"))



# Visualizar estrutura e primeiras linhas
glimpse(dados)
head(dados)

# ---------------------------
# 3. Corrigir erros nos nomes das colunas
# ---------------------------
dados <- dados %>%
  rename(
    Masculino_2023 = `Maculino_2023`,
    Municipio = `Município de notificação`
  )

# Conferir resultado
glimpse(dados)

# ---------------------------
# 4. Identificar valores ausentes (NA)
# ---------------------------
colSums(is.na(dados))

# ---------------------------
# 5. Separar código e nome dos municípios
# ---------------------------
dados <- dados %>%
  mutate(
    codigo = str_extract(Municipio, "\\d{6}"),
    Municipio = str_remove(Municipio, "^\\d{6}\\s*")
  )

# Conferir as primeiras linhas
head(dados$Municipio)
head(dados$codigo)

# ---------------------------
# 6. (Opcional) Visualizar os dados limpos
# ---------------------------
View(dados)
glimpse(dados)

# ============================================================
# 7. Criar novas colunas e comparar os anos
# ============================================================
dados <- dados %>%
  mutate(
    Total_2023 = Negativo_2023 + Positivo_2023,
    Total_2024 = Negativo_2024 + Positivo_2024,
    Dif_Pos = Positivo_2024 - Positivo_2023
  )

glimpse(dados)

# ============================================================
# 8. Identificar municípios com maior e menor variação
# ============================================================
# Top 10 municípios com maior aumento nos casos positivos
dados %>%
  arrange(desc(Dif_Pos)) %>%
  head(10)

# Top 10 municípios com maior redução nos casos positivos
dados %>%
  arrange(Dif_Pos) %>%
  head(10)
