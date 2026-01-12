# =========================================================
# 1. Pacotes
# =========================================================
library(tidyverse)
library(janitor)
library(readr)
library(ggplot2)

# =========================================================
# 2. Carregamento das tabelas
# =========================================================
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

# =========================================================
# 3. Base principal
# =========================================================
alunos_final <- tabelas[["alunos-final"]] %>%
  clean_names()

# Conferência
glimpse(alunos_final)

# =========================================================
# 4. Filtragem e padronização da amostra
# =========================================================
dados_filtrados <- alunos_final %>%
  mutate(
    periodo_de_ingresso = as.numeric(periodo_de_ingresso),
    periodo_de_evasao   = as.numeric(periodo_de_evasao),
    status              = str_to_upper(str_trim(status)),
    tipo_de_evasao      = str_to_upper(str_trim(tipo_de_evasao)),
    forma_de_ingresso   = str_to_upper(str_trim(forma_de_ingresso)),
    sexo                = str_to_upper(str_trim(sexo))
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

nrow(dados_filtrados)

# =========================================================
# 5. Identificação da evasão no 1º período (P1)
# =========================================================
dados_1_periodo <- dados_filtrados %>%
  mutate(
    evadiu_p1 = if_else(
      status == "INATIVO" &
        tipo_de_evasao != "GRADUADO" &
        !is.na(periodo_de_evasao) &
        periodo_de_evasao == periodo_de_ingresso,
      1L, 0L
    )
  )

# =========================================================
# 6. Recorte temporal por currículo
# =========================================================
dados_1_periodo_recorte <- dados_1_periodo %>%
  filter(
    (curriculo == "Currículo 1999" &
       periodo_de_ingresso >= 2011.1 &
       periodo_de_ingresso <= 2017.2) |
      (curriculo == "Currículo 2017" &
         periodo_de_ingresso >= 2018.1 &
         periodo_de_ingresso <= 2023.1)
  )

stopifnot(exists("dados_1_periodo_recorte"))

# =========================================================
# 7. Tabela 5.1 — Taxa de evasão no 1º período (por ingresso)
# =========================================================
tabela_5_1 <- dados_1_periodo_recorte %>%
  group_by(curriculo, periodo_de_ingresso) %>%
  summarise(
    evadidos = sum(evadiu_p1 == 1),
    total = n(),
    taxa_evasao = round((evadidos / total) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(curriculo, periodo_de_ingresso)

tabela_5_1

# =========================================================
# 8. Visão geral por perfil demográfico — SEXO (Seção 5.4.1)
# =========================================================
tabela_sexo_p1 <- dados_1_periodo_recorte %>%
  filter(!is.na(sexo)) %>%
  group_by(curriculo, sexo) %>%
  summarise(
    evadidos = sum(evadiu_p1 == 1),
    total = n(),
    taxa_evasao = round((evadidos / total) * 100, 1),
    .groups = "drop"
  )

tabela_sexo_p1

# =========================================================
# 9. Gráfico comparativo — evasão no P1 por sexo
# =========================================================
ggplot(tabela_sexo_p1,
       aes(x = sexo, y = taxa_evasao, fill = curriculo)) +
  geom_col(position = "dodge") +
  labs(
    title = "Taxa de evasão no 1º período por sexo",
    x = "Sexo",
    y = "Taxa de evasão (%)",
    fill = "Currículo"
  ) +
  theme_minimal()
# =========================================================
# 1. Pacotes
# =========================================================
library(tidyverse)
library(janitor)
library(readr)
library(ggplot2)

# =========================================================
# 2. Carregamento das tabelas
# =========================================================
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

# =========================================================
# 3. Base principal
# =========================================================
alunos_final <- tabelas[["alunos-final"]] %>%
  clean_names()

# Conferência
glimpse(alunos_final)

# =========================================================
# 4. Filtragem e padronização da amostra
# =========================================================
dados_filtrados <- alunos_final %>%
  mutate(
    periodo_de_ingresso = as.numeric(periodo_de_ingresso),
    periodo_de_evasao   = as.numeric(periodo_de_evasao),
    status              = str_to_upper(str_trim(status)),
    tipo_de_evasao      = str_to_upper(str_trim(tipo_de_evasao)),
    forma_de_ingresso   = str_to_upper(str_trim(forma_de_ingresso)),
    sexo                = str_to_upper(str_trim(sexo))
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

nrow(dados_filtrados)

# =========================================================
# 5. Identificação da evasão no 1º período (P1)
# =========================================================
dados_1_periodo <- dados_filtrados %>%
  mutate(
    evadiu_p1 = if_else(
      status == "INATIVO" &
        tipo_de_evasao != "GRADUADO" &
        !is.na(periodo_de_evasao) &
        periodo_de_evasao == periodo_de_ingresso,
      1L, 0L
    )
  )

# =========================================================
# 6. Recorte temporal por currículo
# =========================================================
dados_1_periodo_recorte <- dados_1_periodo %>%
  filter(
    (curriculo == "Currículo 1999" &
       periodo_de_ingresso >= 2011.1 &
       periodo_de_ingresso <= 2017.2) |
      (curriculo == "Currículo 2017" &
         periodo_de_ingresso >= 2018.1 &
         periodo_de_ingresso <= 2023.1)
  )

stopifnot(exists("dados_1_periodo_recorte"))

# =========================================================
# 7. Tabela 5.1 — Taxa de evasão no 1º período (por ingresso)
# =========================================================
tabela_5_1 <- dados_1_periodo_recorte %>%
  group_by(curriculo, periodo_de_ingresso) %>%
  summarise(
    evadidos = sum(evadiu_p1 == 1),
    total = n(),
    taxa_evasao = round((evadidos / total) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(curriculo, periodo_de_ingresso)

tabela_5_1

# =========================================================
# 8. Visão geral por perfil demográfico — SEXO (Seção 5.4.1)
# =========================================================
tabela_sexo_p1 <- dados_1_periodo_recorte %>%
  filter(!is.na(sexo)) %>%
  group_by(curriculo, sexo) %>%
  summarise(
    evadidos = sum(evadiu_p1 == 1),
    total = n(),
    taxa_evasao = round((evadidos / total) * 100, 1),
    .groups = "drop"
  )

tabela_sexo_p1

# =========================================================
# 9. Gráfico comparativo — evasão no P1 por sexo
# =========================================================
ggplot(tabela_sexo_p1,
       aes(x = sexo, y = taxa_evasao, fill = curriculo)) +
  geom_col(position = "dodge") +
  labs(
    title = "Taxa de evasão no 1º período por sexo",
    x = "Sexo",
    y = "Taxa de evasão (%)",
    fill = "Currículo"
  ) +
  theme_minimal()
