# Pacotes
library(tidyverse)
library(janitor)
library(readr)

# Função para carregamento dos dados
pasta_dados <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2025.2/Tabelas"

carregar_tabelas <- function(pasta) {
  arquivos <- list.files(pasta, pattern = "\\.csv$", full.names = TRUE)
  stopifnot(length(arquivos) > 0)
  
  arquivos |>
    set_names(tools::file_path_sans_ext(basename(.))) |>
    map(~ read_delim(
      .x,
      delim = ";",
      show_col_types = FALSE,
      locale = locale(decimal_mark = ".", grouping_mark = ",")
    ))
}

tabelas <- carregar_tabelas(pasta_dados)

# Base principal
alunos_final <- tabelas[["alunos-final"]] |>
  clean_names()

# Filtragem

dados_base <- alunos_final |>
  mutate(
    periodo_de_ingresso = as.numeric(periodo_de_ingresso),
    periodo_de_evasao   = as.numeric(periodo_de_evasao),
    status              = str_to_upper(str_trim(status)),
    tipo_de_evasao      = str_to_upper(str_trim(tipo_de_evasao)),
    forma_de_ingresso   = str_to_upper(str_trim(forma_de_ingresso)),
    curriculo = factor(
      curriculo,
      levels = c(1999, 2017),
      labels = c("Currículo 1999", "Currículo 2017")
    )
  ) |>
  filter(
    periodo_de_ingresso >= 2011.1,
    periodo_de_ingresso <= 2023.2,
    curriculo %in% c("Currículo 1999", "Currículo 2017"),
    !is.na(forma_de_ingresso)
  )


# evasão no 1º período

dados_1_periodo <- dados_base |>
  mutate(
    evadiu_1_periodo = if_else(
      status == "INATIVO" &
        tipo_de_evasao != "GRADUADO" &
        !is.na(periodo_de_evasao) &
        periodo_de_evasao == periodo_de_ingresso,
      1L, 0L
    )
  )

# Recorte temporal por currículo
dados_analise <- dados_1_periodo |>
  filter(
    (curriculo == "Currículo 1999" &
       periodo_de_ingresso >= 2011.1 &
       periodo_de_ingresso <= 2017.2) |
      (curriculo == "Currículo 2017" &
         periodo_de_ingresso >= 2018.1 &
         periodo_de_ingresso <= 2023.1)
  )

# Construção da Tabela 5.1

tabela_5_1 <- dados_analise |>
  group_by(curriculo, periodo_de_ingresso) |>
  summarise(
    ativos   = sum(evadiu_1_periodo == 0),
    evadidos = sum(evadiu_1_periodo == 1),
    total    = ativos + evadidos,
    taxa_evasao = if_else(
      total > 0,
      round((evadidos / total) * 100, 1),
      0
    ),
    .groups = "drop"
  ) |>
  arrange(curriculo, periodo_de_ingresso)

tabela_5_1

# Gráfico de linhas comparativo
ggplot(tabela_5_1,
       aes(x = periodo_de_ingresso,
           y = taxa_evasao,
           color = curriculo,
           group = curriculo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "Taxa de evasão no 1º período por currículo",
    subtitle = "Comparação entre os currículos de 1999 e 2017",
    x = "Período de ingresso",
    y = "Taxa de evasão (%)",
    color = "Currículo"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Gráfico boxplot – evasão no 1º período por currículo

ggplot(tabela_5_1,
       aes(x = curriculo,
           y = taxa_evasao,
           fill = curriculo)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1)
  ) +
  labs(
    title = "Distribuição da taxa de evasão no 1º período por currículo",
    subtitle = "Comparação entre os currículos de 1999 e 2017",
    x = "Currículo",
    y = "Taxa de evasão (%)",
    fill = "Currículo"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )

# Preparação dos dados relativo aos 11 períodos

tabela_5_1_plot <- tabela_5_1 %>%
  group_by(curriculo) %>%
  arrange(periodo_de_ingresso, .by_group = TRUE) %>%
  mutate(periodo_relativo = row_number()) %>%
  ungroup()

# Gráfico de linhas paralelas – evasão no 1º período

ggplot(tabela_5_1_plot,
       aes(x = periodo_relativo,
           y = taxa_evasao,
           color = curriculo,
           group = curriculo)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = 1:11,
    labels = paste0("P", 1:11)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1)
  ) +
  labs(
    title = "Comparação das taxas de evasão no 1º período",
    subtitle = "Currículos 1999 e 2017 (janelas temporais alinhadas)",
    x = "Período relativo na janela de análise",
    y = "Taxa de evasão (%)",
    color = "Currículo"
  ) +
  theme_minimal()


# Quando comparamos periodos relativos aos dois curriculos 10 
tabela_5_1_plot <- tabela_5_1 %>%
  group_by(curriculo) %>%
  arrange(periodo_de_ingresso, .by_group = TRUE) %>%
  mutate(periodo_relativo = row_number()) %>%
  ungroup() %>%
  filter(periodo_relativo <= 10)

###
ggplot(tabela_5_1_plot,
       aes(x = periodo_relativo,
           y = taxa_evasao,
           color = curriculo,
           group = curriculo)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = 1:10,
    labels = paste0("P", 1:10)
  ) +
  labs(
    title = "Comparação das taxas de evasão no 1º período",
    subtitle = "Currículos 1999 e 2017 (períodos relativos comparáveis)",
    x = "Período relativo",
    y = "Taxa de evasão (%)",
    color = "Currículo"
  ) +
  theme_minimal()

