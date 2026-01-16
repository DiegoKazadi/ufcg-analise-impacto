# 1. Pacotes
library(tidyverse)
library(janitor)
library(readr)
library(ggplot2)

# 2. Base principal já carregada e limpa (alunos_final)

dados <- alunos_final %>%
  mutate(
    periodo_de_ingresso = as.numeric(periodo_de_ingresso),
    periodo_de_evasao   = as.numeric(periodo_de_evasao),
    status              = str_to_upper(str_trim(status)),
    tipo_de_evasao      = str_to_upper(str_trim(tipo_de_evasao)),
    forma_de_ingresso   = str_to_upper(str_trim(forma_de_ingresso)),
    faixa_etaria        = str_to_upper(str_trim(idade_aproximada_no_ingresso)),
    curriculo = factor(curriculo,
                       levels = c(1999, 2017),
                       labels = c("Currículo 1999", "Currículo 2017"))
  ) %>%
  filter(!is.na(faixa_etaria), !is.na(forma_de_ingresso))

# 3. Função para calcular evasão por período
calcular_evasao_periodo <- function(df, periodo_num) {
  
  df %>%
    mutate(
      periodo_analise = periodo_num,
      evadiu = case_when(
        periodo_num == 1 ~ status == "INATIVO" & tipo_de_evasao != "GRADUADO" & periodo_de_evasao == periodo_de_ingresso,
        periodo_num == 2 ~ status == "INATIVO" & tipo_de_evasao != "GRADUADO" & periodo_de_evasao == periodo_de_ingresso + 0.5,
        periodo_num == 3 ~ status == "INATIVO" & tipo_de_evasao != "GRADUADO" & periodo_de_evasao == periodo_de_ingresso + 1,
        periodo_num == 4 ~ status == "INATIVO" & tipo_de_evasao != "GRADUADO" & periodo_de_evasao == periodo_de_ingresso + 1.5,
        TRUE ~ FALSE
      )
    )
}

# 4. Aplicar para os 4 períodos e unir
lista_periodos <- lapply(1:4, function(p) calcular_evasao_periodo(dados, p))
dados_periodos <- bind_rows(lista_periodos)

# 5. Ajustar janelas de ingresso por currículo e período
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

# 6. Tabela de evasão por faixa etária e período
tabela_faixa_periodo <- dados_periodos %>%
  group_by(curriculo, periodo_analise, faixa_etaria) %>%
  summarise(
    evadidos = sum(evadiu),
    total = n(),
    taxa_evasao = round((evadidos / total) * 100, 1),
    .groups = "drop"
  )

tabela_faixa_periodo

# 7. Gráfico comparativo — evasão por faixa etária e período
ggplot(tabela_faixa_periodo, aes(x = factor(periodo_analise), y = taxa_evasao,
                                 fill = faixa_etaria)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = taxa_evasao),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3.5) +
  facet_wrap(~ curriculo) +
  labs(
    title = "Taxa de evasão por faixa etária nos 4 primeiros períodos",
    x = "Período de Análise",
    y = "Taxa de evasão (%)",
    fill = "Faixa Etária"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
