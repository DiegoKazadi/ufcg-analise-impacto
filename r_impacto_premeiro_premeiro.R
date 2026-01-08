# ============================================================
# ANÁLISE DA EVASÃO NO 1º PERÍODO
# COMPARAÇÃO ENTRE CURRÍCULO 1999 E CURRÍCULO 2017
# ============================================================

# ------------------------------------------------------------
# 1. Pacotes utilizados
# ------------------------------------------------------------
library(tidyverse)
library(janitor)
library(readr)
library(ggplot2)

# ------------------------------------------------------------
# 2. Carregamento das tabelas
# ------------------------------------------------------------
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

# ------------------------------------------------------------
# 3. Base principal
# ------------------------------------------------------------
alunos_final <- tabelas[["alunos-final"]] %>%
  clean_names()

# ------------------------------------------------------------
# 4. Filtragem e padronização dos dados
# ------------------------------------------------------------
dados_filtrados <- alunos_final %>%
  mutate(
    periodo_de_ingresso = as.numeric(periodo_de_ingresso),
    periodo_de_evasao   = as.numeric(periodo_de_evasao),
    status              = str_to_upper(str_trim(status)),
    tipo_de_evasao      = str_to_upper(str_trim(tipo_de_evasao)),
    forma_de_ingresso   = str_to_upper(str_trim(forma_de_ingresso))
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

# ------------------------------------------------------------
# 5. Identificação da evasão no 1º período
# ------------------------------------------------------------
dados_1_periodo <- dados_filtrados %>%
  mutate(
    evadiu_1_periodo = if_else(
      status == "INATIVO" &
        tipo_de_evasao != "GRADUADO" &
        !is.na(periodo_de_evasao) &
        periodo_de_evasao == periodo_de_ingresso,
      1L, 0L
    )
  )

# ------------------------------------------------------------
# 6. Definição das janelas temporais (mesmo período observado)
# ------------------------------------------------------------
janelas_1_periodo <- tibble(
  curriculo = c(
    rep("Currículo 1999", 13),
    rep("Currículo 2017", 13)
  ),
  periodo_de_ingresso = rep(
    seq(2011.1, 2023.1, by = 0.1),
    times = 2
  )
)

# ------------------------------------------------------------
# 7. Agregação por currículo e período de ingresso
# ------------------------------------------------------------
tabela_1_periodo_bruta <- dados_1_periodo %>%
  group_by(curriculo, periodo_de_ingresso) %>%
  summarise(
    ativos   = sum(evadiu_1_periodo == 0),
    evadidos = sum(evadiu_1_periodo == 1),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 8. Construção da Tabela 5.1 (final)
# ------------------------------------------------------------
tabela_5_1 <- janelas_1_periodo %>%
  left_join(
    tabela_1_periodo_bruta,
    by = c("curriculo", "periodo_de_ingresso")
  ) %>%
  mutate(
    ativos   = replace_na(ativos, 0),
    evadidos = replace_na(evadidos, 0),
    total    = ativos + evadidos,
    taxa_evasao = if_else(
      total > 0,
      round((evadidos / total) * 100, 1),
      NA_real_
    )
  ) %>%
  arrange(curriculo, periodo_de_ingresso)

# ------------------------------------------------------------
# 9. Visualização da Tabela 5.1
# ------------------------------------------------------------
tabela_5_1


ggplot(tabela_5_1,
       aes(x = periodo_de_ingresso,
           y = taxa_evasao,
           color = curriculo,
           group = curriculo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Taxa de Evasão no 1º Período por Currículo",
    x = "Período de Ingresso",
    y = "Taxa de Evasão (%)",
    color = "Currículo"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

