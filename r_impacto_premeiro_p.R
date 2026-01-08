
# Pacotes utilizados
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

# Conferência inicial
colnames(alunos_final)
glimpse(alunos_final)

# 4. Filtragem da amostra (tipagem correta)
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
    ),
    forma_de_ingresso = str_to_upper(str_trim(forma_de_ingresso))
  )

# Tamanho final da base
nrow(dados_filtrados)

# ------------------------------------------------------------
# 5. Identificação da evasão no 1º período
# Considera evasão quando:
# - status do aluno é INATIVO
# - não é GRADUADO
# - período de evasão é conhecido
# - evasão ocorre no mesmo período do ingresso

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

# 6. Definição das janelas temporais analisadas
# As janelas garantem a presença de todos os períodos,
# inclusive aqueles sem evasões registradas

janelas_1_periodo <- tibble(
  curriculo = c(
    rep("Currículo 1999", 11),
    rep("Currículo 2017", 11)
  ),
  periodo_de_ingresso = c(
    seq(2011.1, 2016.2, by = 0.1), 2017.2,
    seq(2018.1, 2022.2, by = 0.1), 2023.1
  )
)

# 7. Agregação dos dados por currículo e período de ingresso

tabela_1_periodo_bruta <- dados_1_periodo %>%
  filter(
    (curriculo == "Currículo 1999" &
       periodo_de_ingresso >= 2011.1 &
       periodo_de_ingresso <= 2017.2) |
      (curriculo == "Currículo 2017" &
         periodo_de_ingresso >= 2018.1 &
         periodo_de_ingresso <= 2023.1)
  ) %>%
  group_by(curriculo, periodo_de_ingresso) %>%
  summarise(
    ativos   = sum(evadiu_1_periodo == 0),
    evadidos = sum(evadiu_1_periodo == 1),
    .groups = "drop"
  )

# 8. Construção da Tabela 5.1
# Inclusão de períodos sem registro e cálculo da taxa de evasão

tabela_5_1 <- janelas_1_periodo %>%
  left_join(
    tabela_1_periodo_bruta,
    by = c("curriculo", "periodo_de_ingresso")
  ) %>%
  mutate(
    ativos   = replace_na(ativos, 0),
    evadidos = replace_na(evadidos, 0),
    taxa_evasao = if_else(
      ativos + evadidos > 0,
      round((evadidos / (ativos + evadidos)) * 100, 1),
      0
    )
  ) %>%
  arrange(curriculo, periodo_de_ingresso)

# 9. Visualização final da Tabela 5.1

tabela_5_1
