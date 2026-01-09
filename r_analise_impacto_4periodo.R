# TABELA 5.4 – EVASÃO AO FINAL DO 4º PERÍODO

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

# 4. Filtragem e padronização
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
    )
  )

# ---------------------------------------------------------
# 5. Identificação da evasão até o 4º período
# Critério metodológico:
# - status == INATIVO
# - tipo_de_evasao != GRADUADO
# - evasão ocorre até o 4º período após o ingresso
# ---------------------------------------------------------

dados_4_periodo <- dados_filtrados %>%
  mutate(
    evadiu_4_periodo = if_else(
      status == "INATIVO" &
        tipo_de_evasao != "GRADUADO" &
        !is.na(periodo_de_evasao) &
        periodo_de_evasao <= (periodo_de_ingresso + 0.3),
      1L, 0L
    )
  )

# ---------------------------------------------------------
# 6. Recorte temporal por currículo
# (garantindo observação completa do 4º período)
# ---------------------------------------------------------

dados_4_periodo_recorte <- dados_4_periodo %>%
  filter(
    (curriculo == "Currículo 1999" &
       periodo_de_ingresso >= 2011.1 &
       periodo_de_ingresso <= 2014.2) |
      (curriculo == "Currículo 2017" &
         periodo_de_ingresso >= 2018.1 &
         periodo_de_ingresso <= 2021.2)
  )

# ---------------------------------------------------------
# 7. Construção da Tabela 5.4
# ---------------------------------------------------------

tabela_5_4 <- dados_4_periodo_recorte %>%
  group_by(curriculo, periodo_de_ingresso) %>%
  summarise(
    ativos   = sum(evadiu_4_periodo == 0),
    evadidos = sum(evadiu_4_periodo == 1),
    taxa_evasao = round(
      (evadidos / (ativos + evadidos)) * 100, 1
    ),
    .groups = "drop"
  ) %>%
  arrange(curriculo, periodo_de_ingresso)

# Visualização da tabela
tabela_5_4

# ---------------------------------------------------------
# Preparação dos dados para o gráfico
# Criação do período relativo (P1 a P8)
# ---------------------------------------------------------

tabela_5_4_grafico <- tabela_5_4 %>%
  group_by(curriculo) %>%
  arrange(periodo_de_ingresso) %>%
  mutate(
    periodo_relativo = row_number()
  ) %>%
  ungroup()

# ---------------------------------------------------------
# Gráfico de linhas paralelas – Tabela 5.4
# ---------------------------------------------------------

ggplot(tabela_5_4_grafico,
       aes(x = periodo_relativo,
           y = taxa_evasao,
           group = curriculo,
           color = curriculo,
           linetype = curriculo)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = 1:8,
    labels = paste0("P", 1:8)
  ) +
  labs(
    title = "Taxa de evasão acumulada até o final do 4º período por currículo",
    x = "Períodos de ingresso (P1–P8)",
    y = "Taxa de evasão (%)",
    color = "Currículo",
    linetype = "Currículo"
  ) +
  theme_minimal()

