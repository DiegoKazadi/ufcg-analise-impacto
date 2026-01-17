library(tidyverse)
library(janitor)
library(readr)
library(ggplot2)
# Base já carregada: alunos_final
dados <- alunos_final %>%
  mutate(
    periodo_de_ingresso = as.numeric(periodo_de_ingresso),
    periodo_de_evasao   = as.numeric(periodo_de_evasao),
    status              = str_to_upper(str_trim(status)),
    tipo_de_evasao      = str_to_upper(str_trim(tipo_de_evasao)),
    curriculo = factor(curriculo,
                       levels = c(1999, 2017),
                       labels = c("Currículo 1999", "Currículo 2017"))
  ) %>%
  filter(!is.na(status), !is.na(tipo_de_evasao), !is.na(periodo_de_evasao))

# Função para identificar evasão por período
calcular_evasao_periodo <- function(df, periodo_num) {
  df %>%
    mutate(
      periodo_analise = factor(periodo_num, levels = 1:4),
      evadiu = case_when(
        periodo_num == 1 ~ status == "INATIVO" & tipo_de_evasao != "GRADUADO" & periodo_de_evasao == periodo_de_ingresso,
        periodo_num == 2 ~ status == "INATIVO" & tipo_de_evasao != "GRADUADO" & periodo_de_evasao == periodo_de_ingresso + 0.5,
        periodo_num == 3 ~ status == "INATIVO" & tipo_de_evasao != "GRADUADO" & periodo_de_evasao == periodo_de_ingresso + 1,
        periodo_num == 4 ~ status == "INATIVO" & tipo_de_evasao != "GRADUADO" & periodo_de_evasao == periodo_de_ingresso + 1.5,
        TRUE ~ FALSE
      )
    )
}

# Aplicar para os 4 períodos e unir
dados_periodos <- bind_rows(lapply(1:4, function(p) calcular_evasao_periodo(dados, p)))

# Ajustar janelas de ingresso por currículo
dados_periodos <- dados_periodos %>%
  mutate(
    valido = case_when(
      curriculo == "Currículo 1999" & periodo_analise == 1 & periodo_de_ingresso <= 2017.2 ~ TRUE,
      curriculo == "Currículo 1999" & periodo_analise == 2 & periodo_de_ingresso <= 2016.1 ~ TRUE,
      curriculo == "Currículo 1999" & periodo_analise == 3 & periodo_de_ingresso <= 2015.2 ~ TRUE,
      curriculo == "Currículo 1999" & periodo_analise == 4 & periodo_de_ingresso <= 2014.2 ~ TRUE,
      curriculo == "Currículo 2017" & periodo_analise == 1 & periodo_de_ingresso >= 2018.1 ~ TRUE,
      curriculo == "Currículo 2017" & periodo_analise == 2 & periodo_de_ingresso >= 2018.1 ~ TRUE,
      curriculo == "Currículo 2017" & periodo_analise == 3 & periodo_de_ingresso >= 2018.1 ~ TRUE,
      curriculo == "Currículo 2017" & periodo_analise == 4 & periodo_de_ingresso >= 2018.1 ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  filter(valido)

# Criar coluna com intervalos de ingresso por período e currículo
dados_periodos <- dados_periodos %>%
  mutate(
    intervalo_periodo = case_when(
      curriculo == "Currículo 1999" & periodo_analise == 1 ~ "2011.1–2017.2",
      curriculo == "Currículo 1999" & periodo_analise == 2 ~ "2011.1–2016.1",
      curriculo == "Currículo 1999" & periodo_analise == 3 ~ "2011.1–2015.2",
      curriculo == "Currículo 1999" & periodo_analise == 4 ~ "2011.1–2014.2",
      curriculo == "Currículo 2017" & periodo_analise == 1 ~ "2018.1–2023.1",
      curriculo == "Currículo 2017" & periodo_analise == 2 ~ "2018.1–2022.2",
      curriculo == "Currículo 2017" & periodo_analise == 3 ~ "2018.1–2022.1",
      curriculo == "Currículo 2017" & periodo_analise == 4 ~ "2018.1–2021.2",
      TRUE ~ NA_character_
    )
  )

# Tabela de evasão por período com intervalos
tabela_evasao_periodo <- dados_periodos %>%
  group_by(curriculo, intervalo_periodo) %>%
  summarise(
    evadidos = sum(evadiu),
    total = n(),
    taxa_evasao = round((evadidos / total) * 100, 1),
    .groups = "drop"
  )

tabela_evasao_periodo

# Gráfico atualizado — eixo X com intervalos de ingresso + valores em cima das barras
ggplot(tabela_evasao_periodo, aes(x = intervalo_periodo, y = taxa_evasao, fill = curriculo)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(aes(label = taxa_evasao), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3.5) +
  labs(
    title = "Taxa de evasão por período (intervalo de ingresso)",
    x = "Intervalo de ingresso por período de análise",
    y = "Taxa de evasão (%)",
    fill = "Currículo"
  ) +
  scale_fill_manual(values = c("Currículo 1999" = "#1f77b4", "Currículo 2017" = "#ff7f0e")) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
