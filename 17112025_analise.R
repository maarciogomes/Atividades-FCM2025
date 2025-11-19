library(readxl)
library(dplyr)
library(geobr)
library(sf)
library(ggplot2)
library(scales) # Para formatar a legenda como 0%

########################### TAXA DE POSITIVIDADE 2024 ###############################

df=read_excel("Cursos/GitHUb/Atividade/Taxa_positividade/Dados_agrupados_colunaLonga.xls", 
              sheet = "Dados_agrupados_colunaLonga"); df


# Supondo que seus dados se chamam 'df'
# Renomeie as colunas para facilitar (R não lida bem com espaços ou números no início)
names(df) <- c("codigo", "municipio", "ano", "negativo", "positivo", 
               "feminino", "masculino", "fx_20_39", "fx_40_59")

### Taxa de positividade
df_analise <- df %>%
  mutate(
    total_testes = positivo + negativo,
    taxa_positividade = (positivo / total_testes)
  ) %>%
  # Se alguns municípios tiverem 0 testes, a taxa pode dar NaN (Not a Number).
  # Vamos tratar isso (opcional, mas recomendado)
  filter(total_testes > 0)

### Ranking dos municípios com maior taxa em << 2024 >>
ranking_2024 <- df_analise %>%
  filter(ano == 2024) %>%
  arrange(desc(taxa_positividade))

print(ranking_2024)

### Ranking dos municípios com maior taxa em << 2023 >>
ranking_2023 <- df_analise %>%
  filter(ano == 2023) %>%
  arrange(desc(taxa_positividade))

print(ranking_2023)


################## DISTRIBUIÇÃO ESPACIAL DE TAXA POSITIVIDADE ##################

sp_shapes <- geobr::read_municipality(code_muni = "SP", year = 2020)

# Coluna de 6 dígitos no 'sp_shapes'
# Transformar 3500105 (numérico) -> "350010" (texto) -> 350010 (numérico)
sp_shapes_mod <- sp_shapes %>%
  mutate(
    # Converte o code_muni (ex: 3500105) para texto
    code_muni_texto = as.character(code_muni), 
    
    # Pega os primeiros 6 caracteres (ex: "350010")
    # Este será o nosso novo código para o join
    codigo_6dig = substr(code_muni_texto, 1, 6),
    
    # Converte de volta para número (para dar join com seu 'codigo' que é numérico)
    # Vamos chamar de 'chave_join' para ficar claro
    chave_join_num = as.numeric(codigo_6dig) 
  )

# 2. Preparar seus dados de 2024
#    Aqui, só precisamos garantir que o seu 'codigo' é numérico
dados_mapa_2024 <- df_analise %>%
  filter(ano == 2024) %>%
  
  # Garante que o 'codigo' (que suspeitamos ser de 6 dígitos) é numérico
  mutate(chave_join_num = as.numeric(codigo)) 

# JOIN usando as chaves corretas
mapa_final_2024 <- left_join(sp_shapes_mod, dados_mapa_2024, by = "chave_join_num")

print(head(mapa_final_2024))


# 2. Preparar seus dados de 2023
#    Aqui, só precisamos garantir que o seu 'codigo' é numérico
dados_mapa_2023 <- df_analise %>%
  filter(ano == 2023) %>%
  
  # Garante que o 'codigo' (que suspeitamos ser de 6 dígitos) é numérico
  mutate(chave_join_num = as.numeric(codigo)) 

# JOIN usando as chaves corretas
mapa_final_2023 <- left_join(sp_shapes_mod, dados_mapa_2023, by = "chave_join_num")

print(head(mapa_final_2023))


# Gráfico para << 2024 >>
ggplot(data = mapa_final_2024) +
  
  # geom_sf desenha os shapes. 'fill' Variável que define a cor.
  geom_sf(aes(fill = taxa_positividade)) +
  
  # 'scale_fill_viridis_c' Escala de cor boa para dados contínuos
  # na.value = "grey90" define uma cor para municípios que não tiveram dados (NA)
  scale_fill_viridis_c(name = "Taxa de Positividade", 
                       labels = scales::percent_format(accuracy = 1), 
                       na.value = "grey90") +
  
  # 'labs' define os títulos
  labs(title = "Taxa de Positividade de Tuberculose por Município (SP, 2024)",
       subtitle = "Porcentagem de testes positivos sobre o total de testes realizados",
       caption = "Fonte: Seus dados") +
  
  # 'theme_void' é um tema limpo, sem eixos, ideal para mapas
  theme_void()

# Gráfico para << 2023 >>
ggplot(data = mapa_final_2023) +
  
  # geom_sf desenha os shapes. 'fill' Variável que define a cor.
  geom_sf(aes(fill = taxa_positividade)) +
  
  # 'scale_fill_viridis_c' Escala de cor boa para dados contínuos
  # na.value = "grey90" define uma cor para municípios que não tiveram dados (NA)
  scale_fill_viridis_c(name = "Taxa de Positividade", 
                       labels = scales::percent_format(accuracy = 1), 
                       na.value = "grey90") +
  
  # 'labs' define os títulos
  labs(title = "Taxa de Positividade de Tuberculose por Município (SP, 2023)",
       subtitle = "Porcentagem de testes positivos sobre o total de testes realizados",
       caption = "Fonte: Seus dados") +
  
  # 'theme_void' é um tema limpo, sem eixos, ideal para mapas
  theme_void()


################# COMPARAÇÃO DE DADOS 2023 & 2024 ##############################

# Seleciona apenas as colunas essenciais de 2024
dados_2024_simples <- dados_mapa_2024 %>%
  select(chave_join_num, taxa_2024 = taxa_positividade)

# Seleciona apenas as colunas essenciais de 2023
dados_2023_simples <- dados_mapa_2023 %>%
  select(chave_join_num, taxa_2023 = taxa_positividade)

# Combina 2024, 2023 e o Shapefile
mapa_comparacao <- sp_shapes_mod %>%
  left_join(dados_2024_simples, by = "chave_join_num") %>%
  left_join(dados_2023_simples, by = "chave_join_num") %>%
  
  # Calcula a diferença: 2024 - 2023
  mutate(delta_taxa = taxa_2024 - taxa_2023)


## MAPA DE COMPARAÇÃO (DELTA)
# Usaremos uma escala de cores divergente (azul para redução, vermelho para aumento)
ggplot(data = mapa_comparacao) +
  geom_sf(aes(fill = delta_taxa)) +
  
  # 'scale_fill_distiller' ou 'scale_fill_gradient2' são ideais para diferenças
  scale_fill_gradient2(
    name = "Delta Taxa (%)", 
    # Formata a legenda como porcentagem com sinal (+) ou (-)
    labels = function(x) scales::percent(x, accuracy = 1, scale = 100), 
    low = "blue", 
    mid = "white", 
    high = "red", 
    midpoint = 0, # O ponto de virada (0) é o branco
    na.value = "grey90"
  ) +
  
  labs(title = "Comparação da Taxa de Positividade: 2024 vs. 2023",
       subtitle = "Variação Percentual (2024 menos 2023): Vermelho = Aumento, Azul = Redução",
       caption = "Fonte: Seus dados") +
  theme_void()
