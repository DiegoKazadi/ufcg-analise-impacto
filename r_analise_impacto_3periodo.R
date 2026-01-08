# TABELA 5.3 – EVASÃO AO FINAL DO 3º PERÍODO

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
# 5. Identificação da evasão até o 3º período
# Critério:
# - status == INATIVO
# - tipo_de_evasao != GRADUADO
# - evasão ocorre até dois períodos após o ingresso
# ---------------------------------------------------------
dados_3_periodo <- dados_filtrados %>%
  mutate(
    evadiu_3_periodo = if_else(
      status == "INATIVO" &
        tipo_de_evasao != "GRADUADO" &
        !is.na(periodo_de_evasao) &
        periodo_de_evasao <= (periodo_de_ingresso + 0.2),
      1L, 0L
    )
  )

# ---------------------------------------------------------
# 6. Recorte temporal por currículo
# Currículo 1999: 2011.1 a 2015.2
# Currículo 2017: 2018.1 a 2022.1
# ---------------------------------------------------------
dados_3_periodo_recorte <- dados_3_periodo %>%
  filter(
    (curriculo == "Currículo 1999" &
       periodo_de_ingresso >= 2011.1 &
       periodo_de_ingresso <= 2015.2) |
      (curriculo == "Currículo 2017" &
         periodo_de_ingresso >= 2018.1 &
         periodo_de_ingresso <= 2022.1)
  )

# ---------------------------------------------------------
# 7. Construção da Tabela 5.3
# ---------------------------------------------------------
tabela_5_3 <- dados_3_periodo_recorte %>%
  group_by(curriculo, periodo_de_ingresso) %>%
  summarise(
    ativos   = sum(evadiu_3_periodo == 0),
    evadidos = sum(evadiu_3_periodo == 1),
    taxa_evasao = round(
      (evadidos / (ativos + evadidos)) * 100, 1
    ),
    .groups = "drop"
  ) %>%
  arrange(curriculo, periodo_de_ingresso)

# Visualização
tabela_5_3


# ---------------------------------------------------------
# Gráfico de linhas paralelas – Tabela 5.3 (3º período)
# Eixo X padronizado: P1 até P9
# ---------------------------------------------------------

library(ggplot2)
library(dplyr)

# 1. Criar índice sequencial dos períodos (P1, P2, ..., P9)
tabela_5_3_plot <- tabela_5_3 %>%
  group_by(curriculo) %>%
  arrange(periodo_de_ingresso) %>%
  mutate(periodo_ordem = row_number()) %>%
  ungroup()

# 2. Gráfico de linhas paralelas
ggplot(
  tabela_5_3_plot,
  aes(
    x = periodo_ordem,
    y = taxa_evasao,
    group = curriculo,
    color = curriculo,
    linetype = curriculo
  )
) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = 1:max(tabela_5_3_plot$periodo_ordem),
    labels = paste0("P", 1:max(tabela_5_3_plot$periodo_ordem))
  ) +
  labs(
    title = "Taxa de evasão acumulada até o final do 3º período por currículo",
    x = "Período letivo (ordem sequencial)",
    y = "Taxa de evasão (%)",
    color = "Currículo",
    linetype = "Currículo"
  ) +
  theme_minimal()

