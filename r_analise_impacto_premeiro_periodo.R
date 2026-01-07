# Pacotes
library(tidyverse)
library(janitor)
library(readr)
library(ggplot2)

# Carregar tabelas

pasta_dados <- "C:/Users/Big Data/Documents/Master UFCG/Semestre 2025.2/Tabelas"

carregar_tabelas <- function(pasta) {
  arquivos <- list.files(path = pasta, pattern = "\\.csv$", full.names = TRUE)
  if (length(arquivos) == 0) stop("Nenhum arquivo CSV encontrado.")
  
  tabelas <- list()
  for (arq in arquivos) {
    nome <- tools::file_path_sans_ext(basename(arq))
    df <- read_delim(
      arq, delim = ";", show_col_types = FALSE,
      locale = locale(decimal_mark = ".", grouping_mark = ",")
    )
    tabelas[[nome]] <- df
  }
  return(tabelas)
}

tabelas <- carregar_tabelas(pasta_dados)
alunos_final <- tabelas[["alunos-final"]] %>% clean_names()

# Visualizar nomes das colunas
colnames(alunos_final)

# Visão geral da estrutura
glimpse(alunos_final)


# Filtragem temporal da amostra

dados_filtrados <- alunos_final %>%
  filter(
    periodo_de_ingresso >= 2011.1,
    periodo_de_ingresso <= 2023.2,
    curriculo %in% c(1999, 2017),
    !is.na(forma_de_ingresso)
  ) %>%
  mutate(
    curriculo = factor(curriculo,
                       levels = c(1999, 2017),
                       labels = c("Currículo 1999", "Currículo 2017")),
    periodo_de_ingresso = as.factor(periodo_de_ingresso),
    forma_de_ingresso = str_to_upper(str_trim(forma_de_ingresso))
  )

# Conferência do tamanho final da base
nrow(dados_filtrados)

# Tratamento da variável Cor/Raça

dados_cor <- dados_filtrados %>%
  filter(!is.na(cor), cor != "") %>%
  mutate(
    cor = str_to_title(str_trim(cor))
  )
