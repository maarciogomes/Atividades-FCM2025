# ======================================================
# 0. Preparação do Ambiente
# ======================================================

# 🚨 PASSO ESSENCIAL PARA CORRIGIR OS ERROS:
# Descomente e execute as linhas abaixo UMA ÚNICA VEZ para instalar os pacotes faltantes.
 #install.packages(c("tidyverse", "readxl", "janitor", "scales"))

#library(tidyverse)
#library(readxl)
#library(janitor)
# O pacote 'scales' é necessário para a formatação de números na Figura 2.
#library(scales) 

# ======================================================
# 1. Importação dos Dados
# ======================================================

# Importa o arquivo Excel, garantindo a limpeza dos nomes das colunas
dados <- read_excel("Dados_agrupados_colunaLonga.xlsx") %>%
  clean_names()

cat("\n## 1. Inspeção dos Dados Importados\n")
glimpse(dados)

# ======================================================
# 2. Transformar dados para formato longo (faixas etárias)
# ======================================================

dados_long <- dados %>%
  # Converte as colunas 'fx_' para o formato longo
  pivot_longer(
    cols = starts_with("fx_"),
    names_to = "faixa_etaria",
    values_to = "casos"
  ) %>%
  # Recodifica os nomes das faixas etárias para um formato mais legível
  mutate(
    faixa_etaria = case_when(
      faixa_etaria == "fx_20_39" ~ "20–39 anos",
      faixa_etaria == "fx_40_59" ~ "40–59 anos",
      TRUE ~ faixa_etaria
    )
  )

cat("\n## 2. Inspeção dos Dados Transformados (Longos)\n")
glimpse(dados_long)

# ======================================================
# 3. FIGURA 1 — Comparação entre faixas etárias (Boxplot)
# ======================================================

# Cria e exibe o gráfico de Boxplot
figura1_boxplot <- ggplot(dados_long, aes(x = faixa_etaria, y = casos, fill = faixa_etaria)) +
  geom_boxplot() +
  labs(
    title = "Comparação da Distribuição de Casos por Faixa Etária",
    x = "Faixa Etária",
    y = "Número de Casos",
    fill = "Faixa Etária"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))

print(figura1_boxplot)
ggsave("Figura_1_Boxplot_Casos_Faixa_Etaria.png", figura1_boxplot, width = 8, height = 5)
cat("\n✅ Figura 1 (Boxplot) salva como Figura_1_Boxplot_Casos_Faixa_Etaria.png\n")


# ======================================================
# 4. FIGURA 2 — Total de casos por faixa etária (Barras)
# ======================================================

# Agrupa e soma o total de casos por faixa etária
dados_agrupados <- dados_long %>%
  group_by(faixa_etaria) %>%
  summarise(total_casos = sum(casos, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(total_casos))

# Cria e exibe o gráfico de Barras
figura2_barras <- dados_agrupados %>%
  ggplot(aes(x = faixa_etaria, y = total_casos, fill = faixa_etaria)) +
  geom_col() +
  # Adiciona os valores no topo das barras, usando a função comma do pacote 'scales'
  geom_text(aes(label = scales::comma(total_casos)), vjust = -0.5, size = 4) +
  labs(
    title = "Total Acumulado de Casos por Faixa Etária",
    x = "Faixa Etária",
    y = "Total de Casos"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  ) +
  # Formata o eixo Y para não usar notação científica
  scale_y_continuous(labels = scales::comma)

print(figura2_barras)
ggsave("Figura_2_Total_Casos_Faixa_Etaria_Barras.png", figura2_barras, width = 8, height = 5)
cat("✅ Figura 2 (Barras) salva como Figura_2_Total_Casos_Faixa_Etaria_Barras.png\n")

