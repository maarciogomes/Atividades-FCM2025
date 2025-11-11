# ============================================================
# SCRIPT: Limpeza e organização dos dados de municípios
# OBJETIVO: Ler, corrigir e estruturar os dados de casos agrupados por município
# AUTOR: [Seu nome ou grupo]
# DATA: [coloque a data da última modificação]
# ============================================================

# ---------------------------
# 1. Carregar pacotes
# ---------------------------
# O tidyverse reúne vários pacotes úteis: dplyr, readr, stringr, tibble, ggplot2, etc.
library(tidyverse)

# ---------------------------
# 2. Importar o arquivo CSV
# ---------------------------
# O parâmetro locale(encoding = "Latin1") evita problemas com acentos e cedilhas.
dados <- read_csv("Dados_agrupados.csv", locale = locale(encoding = "Latin1"))

# Visualizar estrutura e primeiras linhas
glimpse(dados)
head(dados)

# ---------------------------
# 3. Corrigir erros nos nomes das colunas
# ---------------------------
# O arquivo veio com uma coluna "Maculino_2023" (erro de digitação).
# Corrigimos para "Masculino_2023" e padronizamos o nome da coluna de municípios.
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
# Essa função mostra quantos valores faltam em cada coluna.
colSums(is.na(dados))

# ---------------------------
# 5. Separar código e nome dos municípios
# ---------------------------
# Cada entrada na coluna “Municipio” possui um código numérico seguido do nome (ex: “350010 ADAMANTINA”).
# Vamos separar essas duas informações em colunas diferentes:
dados <- dados %>%
  mutate(
    codigo = str_extract(Municipio, "\\d{6}"),   # extrai os 6 primeiros dígitos
    Municipio = str_remove(Municipio, "^\\d{6}\\s*")  # remove o código e o espaço, deixando só o nome
  )

# Conferir as primeiras linhas
head(dados$Municipio) #ou apenas dados$Municipio para ver tudo
head(dados$codigo)

# ---------------------------
# 6. (Opcional) Visualizar os dados limpos
# ---------------------------
# Abre a tabela em formato de planilha no RStudio
View(dados)

# Conferir estrutura final
glimpse(dados)


# ============================================================
# 7. Criar novas colunas e comparar os anos
# ============================================================

# Aqui criamos colunas derivadas dos dados originais:
# - Total_2023 e Total_2024: somam casos positivos e negativos
# - Dif_Pos: calcula a diferença no número de casos positivos entre 2024 e 2023
#   (valores positivos indicam aumento; negativos, redução)
dados <- dados %>%
  mutate(
    Total_2023 = Negativo_2023 + Positivo_2023,
    Total_2024 = Negativo_2024 + Positivo_2024,
    Dif_Pos = Positivo_2024 - Positivo_2023
  )

# Conferir a estrutura atualizada
glimpse(dados)

# ============================================================
# 8. Identificar municípios com maior e menor variação
# ============================================================

# 🔝 Top 10 municípios com maior aumento nos casos positivos
dados %>%
  arrange(desc(Dif_Pos)) %>%
  head(10)

# 🔻 Top 10 municípios com maior redução nos casos positivos
dados %>%
  arrange(Dif_Pos) %>%
  head(10)
