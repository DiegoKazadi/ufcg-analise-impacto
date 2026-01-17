library(tidyverse)
library(janitor)
library(readr)
library(ggplot2)

# Base já carregada: alunos_final
dados_tipo <- alunos_final %>%
  mutate(
    periodo_de_ingresso = as.numeric(periodo_de_ingresso),
    periodo_de_evasao   = as.numeric(periodo_de_evasao),
    status              = str_to_upper(str_trim(status)),
    tipo_de_evasao      = str_to_upper(str_trim(tipo_de_evasao)),
    curriculo = factor(curriculo,
                       levels = c(1999, 2017),
                       labels = c("Currículo 1999", "Currículo 2017"))
  ) %>%
  filter(!is.na(status), !is.na(tipo_de_evasao), !is.na(periodo_de_evasao),
         status == "INATIVO", tipo_de_evasao != "GRADUADO")

# Agrupar por currículo e tipo de evasão
tabela_tipo_evasao <- dados_tipo %>%
  group_by(curriculo, tipo_de_evasao) %>%
  summarise(
    evadidos = n(),
    total = nrow(dados_tipo[dados_tipo$curriculo == curriculo[1],]),
    taxa_evasao = round((evadidos / total) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(curriculo, desc(taxa_evasao))

tabela_tipo_evasao

# Gráfico de barras — taxa de evasão por tipo
ggplot(tabela_tipo_evasao, aes(x = tipo_de_evasao, y = taxa_evasao, fill = curriculo)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(aes(label = taxa_evasao),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3.5) +
  labs(
    title = "Taxa de evasão por tipo de evasão",
    x = "Tipo de evasão",
    y = "Taxa de evasão (%)",
    fill = "Currículo"
  ) +
  scale_fill_manual(values = c("Currículo 1999" = "#1f77b4", "Currículo 2017" = "#ff7f0e")) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
