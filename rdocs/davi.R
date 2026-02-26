source("rdocs/source/packages.R")

library(readxl)
library(dplyr)
library(tidyr)
library(tibble)
library(janitor)
library(stringr)
library(purrr)
library(ggplot2)

theme_estat <- function(discreto = FALSE, ...) { # Adicionamos o argumento 'discreto'
  
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 10),
      axis.title.x = ggplot2::element_text(colour = "black", size = 10),
      axis.text = ggplot2::element_text(colour = "black", size = 10),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      text = element_text(family = "sans", size = 12),
      legend.position = "top",
      ...
    )
  
  # Definimos a escala do eixo X com base no tipo de dado
  if (discreto) {
    escala_x <- scale_x_discrete() 
  } else {
    escala_x <- scale_x_continuous(
      labels = scales::number_format(decimal.mark = ',', big.mark = "."))
  }
  
  return(
    list(
      theme,
      scale_fill_manual(values = cores_estat),
      scale_colour_manual(values = cores_estat),
      escala_x, # X dinâmico agora
      scale_y_continuous(
        labels = scales::number_format(decimal.mark = ',', big.mark = "."))
    )
  )
}

caminho <- "C:/Users/david/Downloads/Eficiência Financeira - Dados finais.xlsx"

# 2. Ler os nomes (Anos na linha 1 e Indicadores na linha 2)
# Pegamos a linha 1 para os anos
anos <- read_excel(caminho, sheet = "Indicadores", n_max = 0) %>% names()
# Pegamos a linha 2 para os indicadores
indicadores <- read_excel(caminho, sheet = "Indicadores", skip = 1, n_max = 0) %>% names()

# 3. Ler os dados ignorando o cabeçalho bagunçado
dados <- read_excel(caminho, sheet = "Indicadores", skip = 2, col_names = FALSE)

# 4. Criar nomes de colunas únicos (Ex: "2024_Receita per capita")
# Ajustamos os nomes das colunas 3 em diante (as 2 primeiras são ID e Nome)
nomes_completos <- c("ID", "Nome")
ano_atual <- ""

for(i in 3:length(anos)) {
  if(!str_detect(anos[i], "[:digit:]{4}")) { 
    # Se o R leu como "...3", mantemos o ano anterior
    anos[i] <- ano_atual 
  } else {
    ano_atual <- anos[i]
  }
  nomes_completos <- c(nomes_completos, paste0(anos[i], "|", indicadores[i]))
}

colnames(dados) <- nomes_completos

# 5. Transformar para o formato Long
df_final <- dados %>%
  pivot_longer(
    cols = -c(ID, Nome),
    names_to = c("Ano", "Indicador"),
    names_sep = "\\|",
    values_to = "Valor"
  )

df_limpo <- df_final %>%
  mutate(
    # 1. Limpa os "..." e tudo depois (sujeira do Excel)
    across(where(is.character), ~ str_remove(., "\\.\\.\\..*")),
    
    # 2. Padronização manual (Snake Case) sem adicionar números
    Indicador = Indicador %>% 
      str_to_lower() %>%                             # Tudo minúsculo
      iconv(to = "ASCII//TRANSLIT") %>%              # Remove acentos
      str_replace_all("[^a-z0-9]+", "_") %>%         # Troca espaços e símbolos por _
      str_remove_all("^_|_$"),                       # Remove _ no início ou fim
    
    # 3. Limpeza final de Ano e espaços
    Ano = as.numeric(str_trim(Ano))
  ) %>%
  filter(!is.na(Ano), Indicador != "")


# receita per capita - não vale a pena o boxplot pq 75% dos dados são exatamente 0,00, fica feio e não diz muito
df_limpo %>%
  filter(Indicador == "receita_per_capita") %>%
  mutate(Valor = as.numeric(Valor)) %>% 
  filter(!is.na(Valor)) %>%  
  group_by(Ano) %>%
  print_quadro_resumo(var_name = Valor)

box18 <- df_limpo %>%
  filter(Indicador == "receita_per_capita") %>%
  mutate(Valor = as.numeric(Valor)) %>% 
  filter(!is.na(Valor)) %>% 
  ggplot() +
  aes(x = as.factor(Ano), y = Valor) +
  geom_boxplot(fill = "#A11D21", width = 0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Ano", y = "Receita per capita") +
  theme_estat(discreto = TRUE)

# participação dos gastos operacionais - não vale a pena o boxplot pq 75% dos dados são exatamente 0,00, fica feio e não diz muito
df_limpo %>%
  filter(Indicador == "participacao_dos_gastos_operacionais") %>%
  mutate(Valor = as.numeric(Valor)) %>% 
  filter(!is.na(Valor)) %>%  
  group_by(Ano) %>%
  print_quadro_resumo(var_name = Valor)

box17 <- df_limpo %>%
  filter(Indicador == "participacao_dos_gastos_operacionais") %>%
  mutate(Valor = as.numeric(Valor)) %>% 
  filter(!is.na(Valor)) %>% 
  ggplot() +
  aes(x = as.factor(Ano), y = Valor) +
  geom_boxplot(fill = "#A11D21", width = 0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Ano", y = "Participação dos gastos operacionais") +
  theme_estat(discreto = TRUE)

# cobertura de despesas
df_limpo %>%
  filter(Indicador == "cobertura_de_despesas") %>%
  mutate(Valor = as.numeric(Valor)) %>% 
  filter(!is.na(Valor)) %>%  
  group_by(Ano) %>%
  print_quadro_resumo(var_name = Valor)

box11 <- df_limpo %>%
  filter(Indicador == "cobertura_de_despesas") %>%
  mutate(Valor = as.numeric(Valor)) %>% 
  filter(!is.na(Valor)) %>% 
  ggplot() +
  aes(x = as.factor(Ano), y = Valor) +
  geom_boxplot(fill = "#A11D21", width = 0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Ano", y = "Índice de Cobertura de Despesas") +
  theme_estat(discreto = TRUE)

# recursos para cobertura de queda de receita
df_limpo %>%
  filter(Indicador == "recursos_para_cobertura_de_queda_de_receita") %>%
  mutate(Valor = as.numeric(Valor)) %>% 
  filter(!is.na(Valor)) %>%  
  group_by(Ano) %>%
  print_quadro_resumo(var_name = Valor)

box12 <- df_limpo %>%
  filter(Indicador == "recursos_para_cobertura_de_queda_de_receita") %>%
  mutate(Valor = as.numeric(Valor)) %>% 
  filter(!is.na(Valor)) %>% 
  ggplot() +
  aes(x = as.factor(Ano), y = Valor) +
  geom_boxplot(fill = "#A11D21", width = 0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Ano", y = "Recursos para cobertura de queda de receita") +
  theme_estat(discreto = TRUE)

# recursos para cobertura de obrigações correntes
df_limpo %>%
  filter(Indicador == "recursos_para_cobertura_de_obrigacoes_correntes") %>%
  mutate(Valor = as.numeric(Valor)) %>% 
  filter(!is.na(Valor)) %>%  
  group_by(Ano) %>%
  print_quadro_resumo(var_name = Valor)

box13 <- df_limpo %>%
  filter(Indicador == "recursos_para_cobertura_de_obrigacoes_correntes") %>%
  mutate(Valor = as.numeric(Valor)) %>% 
  filter(!is.na(Valor)) %>% 
  ggplot() +
  aes(x = as.factor(Ano), y = Valor) +
  geom_boxplot(fill = "#A11D21", width = 0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Ano", y = "Recursos para cobertura de obrigações correntes") +
  theme_estat(discreto = TRUE)


# comprometimento das receitas com as obrigações correntes
df_limpo %>%
  filter(Indicador == "compromentimento_das_receitas_com_as_obrigacoes_correntes") %>%
  mutate(Valor = as.numeric(Valor)) %>% 
  filter(!is.na(Valor)) %>%  
  group_by(Ano) %>%
  print_quadro_resumo(var_name = Valor)

box14 <- df_limpo %>%
  filter(Indicador == "compromentimento_das_receitas_com_as_obrigacoes_correntes") %>%
  mutate(Valor = as.numeric(Valor)) %>% 
  filter(!is.na(Valor)) %>% 
  ggplot() +
  aes(x = as.factor(Ano), y = Valor) +
  geom_boxplot(fill = "#A11D21", width = 0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Ano", y = "Comprometimento das receitas com as obrigações correntes") +
  theme_estat(discreto = TRUE)

# dívida per capita
df_limpo %>%
  filter(Indicador == "divida_per_capita") %>%
  mutate(Valor = as.numeric(Valor)) %>% 
  filter(!is.na(Valor)) %>%  
  group_by(Ano) %>%
  print_quadro_resumo(var_name = Valor)

box15 <- df_limpo %>%
  filter(Indicador == "divida_per_capita") %>%
  mutate(Valor = as.numeric(Valor)) %>% 
  filter(!is.na(Valor)) %>% 
  ggplot() +
  aes(x = as.factor(Ano), y = Valor) +
  geom_boxplot(fill = "#A11D21", width = 0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Ano", y = "Dívida per capita") +
  theme_estat(discreto = TRUE)

# comprometimento das receitas correntes com o endividamento
df_limpo %>%
  filter(Indicador == "comprometimento_das_receitas_correntes_com_o_endividamento") %>%
  mutate(Valor = as.numeric(Valor)) %>% 
  filter(!is.na(Valor)) %>%  
  group_by(Ano) %>%
  print_quadro_resumo(var_name = Valor)

box16 <- df_limpo %>%
  filter(Indicador == "comprometimento_das_receitas_correntes_com_o_endividamento") %>%
  mutate(Valor = as.numeric(Valor)) %>% 
  filter(!is.na(Valor)) %>% 
  ggplot() +
  aes(x = as.factor(Ano), y = Valor) +
  geom_boxplot(fill = "#A11D21", width = 0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Ano", y = "Comprometimento das receitas correntes com o endividamento") +
  theme_estat(discreto = TRUE)







# FATORES DETERMINANTES ---------------------------------------------------------------------------------------------------------------
df <- read_excel("C:/Users/david/Downloads/Fatores determinantes - Dados finais.xlsx", sheet = "Fatores determinantes")
df <- df %>% clean_names()

# Idade
print_quadro_resumo(df, idade)
box10 <- ggplot(df) +
  aes(x = factor(""), y = idade) +
  geom_boxplot(fill = "#A11D21", width = 0.5) +
  guides(fill = "none") + 
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "", y = "Idade") +
  theme_estat(discreto = TRUE)


# população atendida
pop_atend <- setNames(df[2:nrow(df), 7:11], c("2024", "2023","2022","2021","2020"))
pop_atend <- pop_atend %>%
  pivot_longer(
    cols = everything(),
    names_to = "ano",
    values_to = "populacao_atendida"
  )

pop_atend[] <- lapply(pop_atend, as.numeric)
pop_atend %>%
  group_by(ano) %>% # caso mais de uma categoria
  print_quadro_resumo(var_name = populacao_atendida)

box1 <- ggplot(pop_atend) +
     aes(x = as.factor(ano), y = populacao_atendida) +
     geom_boxplot(fill = "#A11D21", width = 0.5) +
     stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
     coord_cartesian(ylim = c(0, 5000000)) + 
     labs(x = "Ano", y = "População atendida") +
     theme_estat(discreto = TRUE)


# Número de municípios consorciados
nmc <- df$numero_de_municipios_consorciados
nmc <- as.data.frame(nmc)
nmc <- nmc[-1, ]
nmc <- as.data.frame(nmc)
nmc %>%
  print_quadro_resumo(var_name = "nmc")

# Quantidade de áreas de atuação
qaa <- df$quantidade_de_areas_de_atuacao
qaa <- as.data.frame(qaa)
qaa <- qaa[-1, ]
qaa <- as.data.frame(qaa)

qaa %>%
  print_quadro_resumo(var_name = "qaa")

box5 <- ggplot(qaa) +
  aes(x = factor(""), y = qaa) +
  geom_boxplot(fill = "#A11D21", width = 0.5) +
  guides(fill = "none") + 
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "", y = "Quantidade de áreas de atuação") +
  theme_estat(discreto = TRUE)

# índice firjan
firjan <- setNames(df[2:nrow(df), 15:18], c("2023","2022","2021","2020"))
firjan <- firjan %>%
  pivot_longer(
    cols = everything(),
    names_to = "ano",
    values_to = "indice_firjan"
  )
firjan$ano <- as.integer(firjan$ano)
firjan %>%
  group_by(ano) %>% # caso mais de uma categoria
  print_quadro_resumo(var_name = indice_firjan)

box9 <- ggplot(firjan) +
  aes(x = as.factor(ano), y = indice_firjan) +
  geom_boxplot(fill = "#A11D21", width = 0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Ano", y = "índice de Firjan") +
  theme_estat(discreto = TRUE)

# saneamento básico: conexão a rede de esgoto (percentual)
saneamento <- df
colnames(saneamento) <- as.character(saneamento[1, ])
saneamento <- saneamento[-1, ]
saneamento <- saneamento[, c(
  "Conexão a rede de esgoto (percentual)",
  "Abastecimento de água pela rede geral (percentual)",
  "Coleta de lixo (percentual)"
)]
saneamento <- janitor::clean_names(saneamento)
saneamento[] <- lapply(saneamento, as.numeric)

saneamento %>%
  print_quadro_resumo(var_name = "conexao_a_rede_de_esgoto_percentual")

box2 <- ggplot(saneamento) +
     aes(x = factor(""), y = conexao_a_rede_de_esgoto_percentual) +
     geom_boxplot(fill = "#A11D21", width = 0.5) +
     guides(fill = "none") + 
     stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
     labs(x = "Conexão à rede de esgoto", y = "Porcentagem (%)") +
     theme_estat(discreto = TRUE)


# saneamento básico: abastecimento de água pela rede geral (percentual)
saneamento %>%
  print_quadro_resumo(var_name = "abastecimento_de_agua_pela_rede_geral_percentual")

box3 <- ggplot(saneamento) +
  aes(x = factor(""), y = abastecimento_de_agua_pela_rede_geral_percentual) +
  geom_boxplot(fill = "#A11D21", width = 0.5) +
  guides(fill = "none") + 
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Abastecimento de água pela rede geral", y = "Percentual (%)") +
  theme_estat(discreto = TRUE)

# saneamento básico: coleta de lixo (percentual)
saneamento %>%
  print_quadro_resumo(var_name = "coleta_de_lixo_percentual")

box4 <- ggplot(saneamento) +
  aes(x = factor(""), y = coleta_de_lixo_percentual) +
  geom_boxplot(fill = "#A11D21", width = 0.5) +
  guides(fill = "none") + 
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Abastecimento de água pela rede geral", y = "Percentual (%)") +
  theme_estat(discreto = TRUE)

# gasto per capita
gasto_cap <- setNames(df[2:nrow(df), 22:26], c("2024","2023","2022","2021","2020"))
gasto_cap <- gasto_cap %>%
  pivot_longer(
    cols = everything(),
    names_to = "ano",
    values_to = "gasto_pc"
  )
gasto_cap[] <- lapply(gasto_cap, as.numeric)
gasto_cap %>%
  group_by(ano) %>% # caso mais de uma categoria
  print_quadro_resumo(var_name = gasto_pc)

box6 <- ggplot(gasto_cap) +
  aes(x = as.factor(ano), y = gasto_pc) +
  geom_boxplot(fill = "#A11D21", width = 0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Ano", y = "Gasto per capita") +
  theme_estat(discreto = TRUE)


# despesa com pessoal/despesa total
despesas <- setNames(df[2:nrow(df), 32:36], c("2024","2023","2022","2021","2020"))
despesas <- despesas %>%
  pivot_longer(
    cols = everything(),
    names_to = "ano",
    values_to = "despesa_pessoal_total"
  )
despesas[] <- lapply(despesas, as.numeric)
despesas %>%
  group_by(ano) %>% # caso mais de uma categoria
  print_quadro_resumo(var_name = despesa_pessoal_total)

box7 <- ggplot(despesas) +
  aes(x = as.factor(ano), y = despesa_pessoal_total) +
  geom_boxplot(fill = "#A11D21", width = 0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Ano", y = "Despesa com pessoal/Despesa Total") +
  theme_estat(discreto = TRUE)

# produção ambulatorial per capita
ambulatorial <- setNames(df[2:nrow(df), 37:41], c("2024","2023","2022","2021","2020"))
ambulatorial <- ambulatorial %>%
  pivot_longer(
    cols = everything(),
    names_to = "ano",
    values_to = "ambulatorial_pc"
  )
ambulatorial[] <- lapply(ambulatorial, as.numeric)
ambulatorial %>%
  group_by(ano) %>% # caso mais de uma categoria
  print_quadro_resumo(var_name = ambulatorial_pc)

box8 <- ggplot(ambulatorial) +
  aes(x = as.factor(ano), y = ambulatorial_pc) +
  geom_boxplot(fill = "#A11D21", width = 0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Ano", y = "Produção ambulatorial per capita") +
  theme_estat(discreto = TRUE)

