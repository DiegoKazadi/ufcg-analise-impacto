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

# =========================================================
# 4. Filtragem e padronização
# =========================================================
dados <- alunos_final %>%
  mutate(
    periodo_de_ingresso = as.numeric(periodo_de_ingresso),
    periodo_de_evasao   = as.numeric(periodo_de_evasao),
    status              = str_to_upper(str_trim(status)),
    tipo_de_evasao      = str_to_upper(str_trim(tipo_de_evasao))
  ) %>%
  filter(
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

# =========================================================
# 5. Função para calcular taxa de evasão por período
# =========================================================
calcular_taxa <- function(base, limite, periodo_label) {
  base %>%
    mutate(
      evadiu = if_else(
        status == "INATIVO" &
          tipo_de_evasao != "GRADUADO" &
          !is.na(periodo_de_evasao) &
          periodo_de_evasao <= (periodo_de_ingresso + limite),
        1L, 0L
      )
    ) %>%
    group_by(curriculo, periodo_de_ingresso) %>%
    summarise(
      evadidos = sum(evadiu),
      total = n(),
      taxa_evasao = (evadidos / total) * 100,
      .groups = "drop"
    ) %>%
    mutate(periodo = periodo_label)
}

# =========================================================
# 6. Aplicação dos recortes corretos (Tabelas 5.1–5.4)
# =========================================================

# 1º período
p1 <- dados %>%
  filter(
    (curriculo == "Currículo 1999" & periodo_de_ingresso >= 2011.1 & periodo_de_ingresso <= 2017.2) |
      (curriculo == "Currículo 2017" & periodo_de_ingresso >= 2018.1 & periodo_de_ingresso <= 2023.1)
  ) %>%
  calcular_taxa(limite = 0.0, periodo_label = "P1")

# 2º período
p2 <- dados %>%
  filter(
    (curriculo == "Currículo 1999" & periodo_de_ingresso >= 2011.1 & periodo_de_ingresso <= 2015.2) |
      (curriculo == "Currículo 2017" & periodo_de_ingresso >= 2018.1 & periodo_de_ingresso <= 2022.2)
  ) %>%
  calcular_taxa(limite = 0.1, periodo_label = "P2")

# 3º período
p3 <- dados %>%
  filter(
    (curriculo == "Currículo 1999" & periodo_de_ingresso >= 2011.1 & periodo_de_ingresso <= 2015.2) |
      (curriculo == "Currículo 2017" & periodo_de_ingresso >= 2018.1 & periodo_de_ingresso <= 2022.1)
  ) %>%
  calcular_taxa(limite = 0.2, periodo_label = "P3")

# 4º período
p4 <- dados %>%
  filter(
    (curriculo == "Currículo 1999" & periodo_de_ingresso >= 2011.1 & periodo_de_ingresso <= 2014.2) |
      (curriculo == "Currículo 2017" & periodo_de_ingresso >= 2018.1 & periodo_de_ingresso <= 2021.2)
  ) %>%
  calcular_taxa(limite = 0.3, periodo_label = "P4")

# =========================================================
# 7. Base final para o boxplot
# =========================================================
boxplot_dados <- bind_rows(p1, p2, p3, p4) %>%
  mutate(
    periodo = factor(periodo, levels = c("P1", "P2", "P3", "P4"))
  )

# =========================================================
# 8. Gráfico Boxplot – Taxa de evasão (P1 a P4)
# =========================================================
ggplot(boxplot_dados,
       aes(x = periodo,
           y = taxa_evasao,
           fill = curriculo)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16) +
  labs(
    title = "Distribuição das taxas de evasão por período letivo",
    x = "Período do curso",
    y = "Taxa de evasão (%)",
    fill = "Currículo"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top"
  )

# =========================================================
# 8. Gráfico Boxplot – Taxa de evasão (P1 a P4)
# =========================================================
ggplot(
  boxplot_dados,
  aes(
    x = periodo,
    y = taxa_evasao,
    fill = curriculo
  )
) +
  geom_boxplot(
    alpha = 0.75,
    outlier.shape = 16,
    outlier.size = 2
  ) +
  scale_fill_manual(
    values = c("#F8766D", "#00BFC4")
  ) +
  labs(
    title = "Distribuição das taxas de evasão por período do curso",
    subtitle = "Comparação entre os currículos de 1999 e 2017",
    x = "Período do curso",
    y = "Taxa de evasão (%)",
    fill = "Currículo"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10)
  )

# =========================================================
# 9. Estatísticas descritivas (média e desvio padrão)
# =========================================================
media_dados <- boxplot_dados %>%
  group_by(curriculo, periodo) %>%
  summarise(
    media_taxa = mean(taxa_evasao, na.rm = TRUE),
    desvio = sd(taxa_evasao, na.rm = TRUE),
    .groups = "drop"
  )

# =========================================================
# 10. Gráfico de linhas paralelas com pontos e barras de erro
# =========================================================
ggplot(media_dados,
       aes(x = periodo,
           y = media_taxa,
           group = curriculo,
           color = curriculo)) +
  
  # Linhas
  geom_line(linewidth = 1) +
  
  # Pontos marcados
  geom_point(size = 3) +
  
  # Barras de erro (desvio padrão)
  geom_errorbar(
    aes(ymin = media_taxa - desvio,
        ymax = media_taxa + desvio),
    width = 0.15,
    linewidth = 0.6
  ) +
  
  labs(
    title = "Média da taxa de evasão por currículo e período",
    x = "Período do curso",
    y = "Taxa média de evasão (%)",
    color = "Currículo"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    #plot.title = element_text(hjust = 0.5, face = "bold")
  )


