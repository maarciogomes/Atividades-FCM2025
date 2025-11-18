library(tidyverse)
library(stringr)

dados <- read_csv2("Dados_agrupados.csv", locale = locale(encoding = "Latin1"))

glimpse(dados)

# 1) Corrigir nomes de colunas com erro de digitação
dados_corr <- dados %>%
  rename_with(~ str_replace_all(., regex("Maculino", ignore_case = TRUE), "Masculino"))

# 2) Extrair só o nome da cidade (remover prefixo numérico + espaços)
dados_corr <- dados_corr %>%
  mutate(
    Municipio = `Município de notificação` %>%
      # remove números no começo e espaços (ex: "350010 " ou "350010-")
      str_remove_all("^\\s*\\d+\\s*[-]?\\s*") %>%
      str_trim()
  )

# "ADAMANTINA" -> "Adamantina")
# use se quiser nomes com primeira letra maiúscula:
dados_corr <- dados_corr %>%
  mutate(Municipio = str_to_title(tolower(Municipio)))

# 3) Selecionar colunas de interesse (exemplos)
#   a) manter apenas Municipio e colunas que começam com "Positivo", "Negativo", "Feminino", "Masculino"
dados_sel <- dados_corr %>%
  select(Municipio, starts_with("Positivo"), starts_with("Negativo"),
         starts_with("Feminino"), starts_with("Masculino"))

#4) Criando variável que representa a diferença de casos positivos dde 2024 - 2023
dados_diff <- dados_corr %>%
  mutate(
    Dif1 = Positivo_2024 - Positivo_2023,
    Dif2 = Positivo_2023 - Positivo_2024
  )

#5) Filtrar as 10 cidades que mais aumentaram os casos
top_aumento <- dados_diff %>%
  arrange(desc(Dif1)) %>%
  slice(1:10)

top_reducao <- dados_diff %>%
  arrange(desc(Dif2)) %>%
  slice(1:10)


library(ggplot2)

#Gráfico dos aumentos
ggplot(top_aumento, aes(x = reorder(Municipio, Dif1), y = Dif1)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Municípios com Maior Aumento de Casos Positivos (2023 → 2024)",
    x = "Município",
    y = "Aumento de casos"
  ) +
  theme_minimal()

#Gráfico das reduções 
ggplot(top_reducao, aes(x = reorder(Municipio, Dif2), y = Dif2)) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  labs(
    title = "Municípios com Maior Redução de Casos Positivos (2023 → 2024)",
    x = "Município",
    y = "Redução de casos"
  ) +
  theme_minimal()

# 6) RANKING completo (do maior aumento à maior redução)
ranking <- dados_diff %>%
  arrange(desc(Dif1))
