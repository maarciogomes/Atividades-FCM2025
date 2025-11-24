# ======================================
# 0. Preparação do Ambiente
# ======================================

# Verificar se estou no projeto correto: "Atividades-FCM2025-RStudio"

library(tidyverse)   # Manipulação e visualização de dados
library(stringr)     # Manipulação de texto

# ======================================
# 1. Importação dos Dados
# ======================================

# read_csv2() lê CSV com separador ";" e decimal ","
# locale(encoding="Latin1") garante leitura correta de acentos (ã, ç, é...)
dados <- read_csv2("Dados_agrupados.csv",
                   locale = locale(encoding = "Latin1"))

glimpse(dados)

# ======================================
# 2. Limpeza dos Dados
# ======================================

dados <- dados %>%
  
  # 2.1 Corrigir erro de digitação em nome de coluna
  rename(Masculino = Maculino_2023) %>% 
  
  # 2.2 Limpar coluna de município
  mutate(
    Municipio = `Município de notificação` %>%
      str_remove_all("^\\s*\\d+\\s*[-]?\\s*") %>%  # remove prefixos numéricos
      str_trim() %>%                               # remove espaços extras
      str_to_title()                               # "ADAMANTINA" → "Adamantina"
  )

# ======================================
# 3. Seleção de Colunas de Interesse
# ======================================

dados_sel <- dados %>%
  select(
    Municipio,
    starts_with("Positivo"),
    starts_with("Negativo")
  )

# ======================================
# 4. Diferença de casos positivos (2024 - 2023)
# ======================================

dados_diff <- dados_sel %>%
  mutate(
    Dif = Positivo_2024 - Positivo_2023
  )

# ======================================
# 5. Top cidades com maior aumento e maior redução
# ======================================

top_aumento <- dados_diff %>%
  arrange(desc(Dif)) %>%
  slice(1:10)

top_reducao <- dados_diff %>%
  arrange(Dif) %>%   # menor Dif = maior redução
  slice(1:10)

# ======================================
# 6. Gráficos
# ======================================

# Tema único
tema <- theme_minimal(base_size = 12)

library(ggplot2)
# Aumento
graf_aumento <- ggplot(top_aumento,
                       aes(x = reorder(Municipio, Dif), y = Dif)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  geom_text(aes(label = Dif),
            hjust = -0.1, size = 3.8) +
  labs(
    title = "Top 10 Municípios com Maior Aumento de Casos Positivos (2023 → 2024)",
    x = "Município",
    y = "Aumento de casos"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  ) +
  ylim(0, max(top_aumento$Dif, na.rm = TRUE) * 1.15)

graf_aumento

# Redução
library(scales)  # para pretty_breaks()

graf_reducao <- ggplot(top_reducao,
                       aes(x = reorder(Municipio, -Dif), y = Dif)) +
  geom_col(fill="#ee9a00") +
  coord_flip() +
  geom_text(aes(label = Dif),
            hjust = -0.1, size = 3.8) +
  labs(
    title = "Top 10 Municípios com Maior Redução de Casos Positivos (2023 → 2024)",
    x = "Município",
    y = "Redução de casos"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title  = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(breaks = pretty_breaks()) 

graf_reducao

# ======================================
# 7. Ranking completo
# ======================================

ranking <- dados_diff %>%
  mutate(
    Dif = Positivo_2024 - Positivo_2023,
    Classificacao = case_when(
      Dif > 0 ~ "Aumento",
      Dif < 0 ~ "Redução",
      TRUE    ~ "Estável"
    )
  ) %>%
  arrange(desc(Dif))

