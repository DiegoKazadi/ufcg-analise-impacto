# TABELA 5.2 – EVASÃO AO FINAL DO 2º PERÍODO

# 1. Pacotes
library(tidyverse)
library(janitor)
library(readr)
library(ggplot2)

# 2. Carregamento das tabelas
pasta_dados <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2025.2/Tabelas"

carregar_tabelas <- function(pasta) {
  arquivos <- list.files(path = pasta, pattern = "\\.csv$", full.names = TRUE)
  if (length(arquivos) == 0) stop("Nenhum arquivo CSV encontrado.")
  
  tabelas <- list()
  for (arq in arquivos) {
    nome <- tools::file_path_sans_ext(basename(arq))
    df <- read_delim(
      arq,
      delim = ";",
      show_col_types = FALSE,
      locale = locale(decimal_mark = ".", grouping_mark = ",")
    )
    tabelas[[nome]] <- df
  }
  return(tabelas)
}

tabelas <- carregar_tabelas(pasta_dados)

# 3. Base principal
alunos_final <- tabelas[["alunos-final"]] %>%
  clean_names()

# Conferência inicial
colnames(alunos_final)
glimpse(alunos_final)

# 4. Filtragem da amostra (padronização e tipagem)
dados_filtrados <- alunos_final %>%
  mutate(
    periodo_de_ingresso = as.numeric(periodo_de_ingresso),
    periodo_de_evasao   = as.numeric(periodo_de_evasao),
    status              = str_to_upper(str_trim(status)),
    tipo_de_evasao      = str_to_upper(str_trim(tipo_de_evasao))
  ) %>%
  filter(
    periodo_de_ingresso >= 2011.1,
    periodo_de_ingresso <= 2023.2,
    curriculo %in% c(1999, 2017),
    !is.na(forma_de_ingresso)
  ) %>%
  mutate(
    curriculo = factor(
      curriculo,
      levels = c(1999, 2017),
      labels = c("Currículo 1999", "Currículo 2017")
    ),
    forma_de_ingresso = str_to_upper(str_trim(forma_de_ingresso))
  )

# Tamanho final da base
nrow(dados_filtrados)

# ---------------------------------------------------------
# 5. Identificação da evasão no 2º período
# Critério metodológico:
# - status == INATIVO
# - tipo_de_evasao != GRADUADO
# - evasão ocorre no período seguinte ao ingresso
# ---------------------------------------------------------
dados_2_periodo <- dados_filtrados %>%
  mutate(
    evadiu_2_periodo = if_else(
      status == "INATIVO" &
        tipo_de_evasao != "GRADUADO" &
        !is.na(periodo_de_evasao) &
        periodo_de_evasao <= (periodo_de_ingresso + 0.1),
      1L, 0L
    )
  )

# 6. Recorte temporal por currículo
# (garantindo observação completa do 2º período)
dados_2_periodo_recorte <- dados_2_periodo %>%
  filter(
    (curriculo == "Currículo 1999" &
       periodo_de_ingresso >= 2011.1 &
       periodo_de_ingresso <= 2015.2) |
      (curriculo == "Currículo 2017" &
         periodo_de_ingresso >= 2018.1 &
         periodo_de_ingresso <= 2022.2)
  )

# 7. Construção da Tabela 5.2 (ambos os currículos)
tabela_5_2 <- dados_2_periodo_recorte %>%
  group_by(curriculo, periodo_de_ingresso) %>%
  summarise(
    ativos   = sum(evadiu_2_periodo == 0),
    evadidos = sum(evadiu_2_periodo == 1),
    taxa_evasao = round(
      (evadidos / (ativos + evadidos)) * 100, 1
    ),
    .groups = "drop"
  ) %>%
  arrange(curriculo, periodo_de_ingresso)

# Visualização da tabela completa
tabela_5_2

# 8. Tabela específica — Currículo 2017 (separada)
tabela_5_2_2017 <- dados_2_periodo_recorte %>%
  filter(curriculo == "Currículo 2017") %>%
  group_by(periodo_de_ingresso) %>%
  summarise(
    ativos   = sum(evadiu_2_periodo == 0),
    evadidos = sum(evadiu_2_periodo == 1),
    taxa_evasao = round(
      (evadidos / (ativos + evadidos)) * 100, 1
    ),
    .groups = "drop"
  ) %>%
  arrange(periodo_de_ingresso)

tabela_5_2_2017

# 9. Gráfico de linhas paralelas – Tabela 5.2
ggplot(tabela_5_2,
       aes(x = as.factor(periodo_de_ingresso),
           y = taxa_evasao,
           group = curriculo,
           linetype = curriculo)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Taxa de evasão ao final do 2º período por currículo",
    x = "Período de ingresso",
    y = "Taxa de evasão (%)",
    linetype = "Currículo"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

