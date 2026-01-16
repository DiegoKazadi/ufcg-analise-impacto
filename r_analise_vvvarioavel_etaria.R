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
    idade_ingresso      = as.numeric(idade_aproximada_no_ingresso),
    curriculo = factor(curriculo,
                       levels = c(1999, 2017),
                       labels = c("Currículo 1999", "Currículo 2017"))
  ) %>%
  filter(!is.na(idade_ingresso), !is.na(forma_de_ingresso))

# 3. Criar intervalos de idade
dados <- dados %>%
  mutate(
    faixa_idade = case_when(
      idade_ingresso < 18 ~ "<18",
      idade_ingresso >= 18 & idade_ingresso <= 20 ~ "18–20",
      idade_ingresso >= 21 & idade_ingresso <= 23 ~ "21–23",
      idade_ingresso >= 24 ~ "24+",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(faixa_idade))

# 4. Função para calcular evasão por período
calcular_evasao_periodo <- function(df, periodo_num) {
  df %>%
    mutate(
      periodo_analise = factor(periodo_num, levels = 1:4), # garante ordem no gráfico
      evadiu = case_when(
        periodo_num == 1 ~ status == "INATIVO" & tipo_de_evasao != "GRADUADO" & periodo_de_evasao == periodo_de_ingresso,
        periodo_num == 2 ~ status == "INATIVO" & tipo_de_evasao != "GRADUADO" & periodo_de_evasao == periodo_de_ingresso + 0.5,
        periodo_num == 3 ~ status == "INATIVO" & tipo_de_evasao != "GRADUADO" & periodo_de_evasao == periodo_de_ingresso + 1,
        periodo_num == 4 ~ status == "INATIVO" & tipo_de_evasao != "GRADUADO" & periodo_de_evasao == periodo_de_ingresso + 1.5,
        TRUE ~ FALSE
      )
    )
}

# 5. Aplicar para os 4 períodos e unir
lista_periodos <- lapply(1:4, function(p) calcular_evasao_periodo(dados, p))
dados_periodos <- bind_rows(lista_periodos)

# 6. Ajustar janelas de ingresso por currículo
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

# 7. Gráfico boxplot — distribuição da idade de evadidos (garante todos períodos)
# Criamos um fator completo com todos os períodos para preencher mesmo sem evasão
periodos_completos <- factor(1:4, levels = 1:4)
dados_periodos$periodo_analise <- factor(dados_periodos$periodo_analise, levels = levels(periodos_completos))

ggplot(dados_periodos %>% filter(evadiu == TRUE), 
       aes(x = periodo_analise, y = idade_ingresso)) +
  geom_boxplot(aes(fill = curriculo)) +
  facet_wrap(~ curriculo) +
  scale_x_discrete(drop = FALSE) + # garante exibição de todos os níveis
  labs(
    title = "Idade de ingresso dos estudantes evadiram nos 4 períodos",
    x = "Período de Análise",
    y = "Idade no Ingresso",
    fill = "Currículo"
  ) +
  theme_minimal(base_size = 12)
