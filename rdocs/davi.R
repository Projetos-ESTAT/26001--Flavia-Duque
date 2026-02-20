source("rdocs/source/packages.R")

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# Ler apenas a aba "Indicadores"
dados_raw <- read_excel("C:/Users/david/Downloads/Eficiência Financeira - Dados finais.xlsx", sheet = "Indicadores")

# Remover a primeira linha (nomes dos indicadores)
dados <- dados_raw[-1, ]

# Separar identificadores
ids <- dados[,1:2]
valores <- dados[,3:ncol(dados)]

# Converter todas as variáveis para numérico
valores <- valores %>% mutate(across(everything(), as.numeric))

# Estatísticas descritivas
descr <- valores %>%
  summarise(across(everything(),
                   list(
                     n = ~sum(!is.na(.)),
                     media = ~mean(., na.rm=TRUE),
                     mediana = ~median(., na.rm=TRUE),
                     sd = ~sd(., na.rm=TRUE),
                     min = ~min(., na.rm=TRUE),
                     max = ~max(., na.rm=TRUE)
                   )))

descr
