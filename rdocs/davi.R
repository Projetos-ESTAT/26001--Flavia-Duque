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

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

# 1. Caminho do arquivo
#caminho <- "C:/Users/david/Downloads/Eficiência Financeira - Dados Finais corrigida 2 (2).xlsx"
caminho<- "C:/Users/DELL/Downloads/Eficiência Financeira - Dados Finais corrigida 2 (2).xlsx"
# 2. Ler planilha inteira sem assumir cabeçalho
raw <- read_excel(caminho, sheet = "Indicadores", col_names = FALSE)

# 3. Separar linhas do cabeçalho
anos <- as.character(unlist(raw[1, ]))
indicadores <- as.character(unlist(raw[2, ]))

# 4. Dados começam na linha 3
dados <- raw[-c(1,2), ]

# 5. Preencher anos corretamente (carregar último ano válido)
ano_atual <- ""
for(i in seq_along(anos)) {
  
  if(!is.na(anos[i]) && str_detect(anos[i], "[0-9]{4}")) {
    ano_atual <- anos[i]
  } else {
    anos[i] <- ano_atual
  }
}

# 6. Criar nomes de colunas seguros
nomes_completos <- c("ID", "Nome")

for(i in 3:length(anos)) {
  
  indicador_atual <- ifelse(
    is.na(indicadores[i]) | indicadores[i] == "",
    paste0("coluna_", i),
    indicadores[i]
  )
  
  nomes_completos <- c(
    nomes_completos,
    paste0(anos[i], "|", indicador_atual)
  )
}

# 7. Aplicar nomes
colnames(dados) <- nomes_completos

# 8. Remover possíveis colunas com nome NA (blindagem extra)
dados <- dados[, !is.na(colnames(dados))]

# 9. Transformar para formato Long
df_final <- dados %>%
  pivot_longer(
    cols = -c(ID, Nome),
    names_to = c("Ano", "Indicador"),
    names_sep = "\\|",
    values_to = "Valor"
  )

# 10. Limpeza final
df_limpo <- df_final %>%
  mutate(
    
    # Limpa sujeira tipo "...3"
    across(where(is.character), ~ str_remove(., "\\.\\.\\..*")),
    
    # Padroniza Indicador em snake_case
    Indicador = Indicador %>%
      str_to_lower() %>%
      iconv(to = "ASCII//TRANSLIT") %>%
      str_replace_all("[^a-z0-9]+", "_") %>%
      str_remove_all("^_|_$"),
    
    Ano = as.numeric(str_trim(Ano))
    
  ) %>%
  filter(!is.na(Ano), Indicador != "")

# Resultado final
df_limpo


# receita per capita
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
#box18
# participação dos gastos operacionais
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
#box17
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
#box11
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
#box12
# despesa com pessoal/despesa total
df_limpo %>%
  filter(Indicador == "despesa_com_pessoal_despesa_total") %>%
  mutate(Valor = as.numeric(Valor)) %>% 
  filter(!is.na(Valor)) %>%  
  group_by(Ano) %>%
  print_quadro_resumo(var_name = Valor)

box19 <- df_limpo %>%
  filter(Indicador == "despesa_com_pessoal_despesa_total") %>%
  mutate(Valor = as.numeric(Valor)) %>% 
  filter(!is.na(Valor)) %>% 
  ggplot() +
  aes(x = as.factor(Ano), y = Valor) +
  geom_boxplot(fill = "#A11D21", width = 0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Ano", y = "Despesa com pessoal/Despesa total") +
  theme_estat(discreto = TRUE)
#box19
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

#box13
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

#box14
# FATORES DETERMINANTES ---------------------------------------------------------------------------------------------------------------
#df <- read_excel("C:/Users/david/Downloads/Fatores determinantes - Dados finais.xlsx", sheet = "Fatores determinantes")
#df<- read_excel("C:/ESTAT/2026001-Flavia/Fatores determinantes - Dados finais.xlsx", sheet= "Fatores determinantes")
df<- read_excel("C:/Users/DELL/Downloads/Fatores determinantes - Dados finais (1).xlsx", sheet= "Fatores determinantes")
df <- df %>% clean_names()

# Idade
df_idade <- df %>% filter(!is.na(idade))
print_quadro_resumo(df_idade, idade)
box10 <- ggplot(df_idade) +
  aes(x = factor(""), y = idade) +
  geom_boxplot(fill = "#A11D21", width = 0.5) +
  guides(fill = "none") + 
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "", y = "Idade") +
  theme_estat(discreto = TRUE)
box10

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

box1
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
qaa <- na.omit(qaa)

qaa %>%
  print_quadro_resumo(var_name = "qaa")

box5 <- ggplot(qaa) +
  aes(x = factor(""), y = qaa) +
  geom_boxplot(fill = "#A11D21", width = 0.5) +
  guides(fill = "none") + 
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "", y = "Quantidade de áreas de atuação") +
  theme_estat(discreto = TRUE)
box5
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
box9
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

#box2
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
box3
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
box4
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

box6
# despesa com pessoal/despesa total
despesas <- setNames(df[2:nrow(df), 27:31], c("2024","2023","2022","2021","2020"))
despesas <- despesas %>%
  pivot_longer(
    cols = everything(),
    names_to = "ano",
    values_to = "despesa_pessoal_total",
    values_drop_na = TRUE  
  )
despesas[] <- lapply(despesas, as.numeric)
despesas %>%
  group_by(ano) %>% # caso mais de uma categoria
  print_quadro_resumo(var_name = despesa_pessoal_total)

box7 <- despesas %>% 
  filter(!is.na(despesa_pessoal_total)) %>% # Limpa antes de entrar no ggplot
  ggplot() +
  aes(x = as.factor(ano), y = despesa_pessoal_total) +
  geom_boxplot(fill = "#A11D21", width = 0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Ano", y = "Despesa com pessoal/Despesa Total") +
  theme_estat(discreto = TRUE)
box7
# produção ambulatorial per capita
#ambulatorial <- setNames(df[2:nrow(df), 37:41], c("2024","2023","2022","2021","2020"))
#ambulatorial <- ambulatorial %>%
 # pivot_longer(
  #  cols = everything(),
   # names_to = "ano",
    #values_to = "ambulatorial_pc"
 # )
#ambulatorial[] <- lapply(ambulatorial, as.numeric)
#ambulatorial %>%
 # group_by(ano) %>% # caso mais de uma categoria
  #print_quadro_resumo(var_name = ambulatorial_pc)

#ncol_df <- ncol(df)
#ambulatorial <- df[2:nrow(df), (ncol_df-4):ncol_df] 
#colnames(ambulatorial) <- c("2024","2023","2022","2021","2020")

#ambulatorial_long <- ambulatorial %>%
 # pivot_longer(cols = everything(), names_to = "ano", values_to = "ambulatorial_pc") %>%
  #mutate(ambulatorial_pc = as.numeric(ambulatorial_pc)) %>%
  #filter(!is.na(ambulatorial_pc))

#ambulatorial_long %>%
 # group_by(ano) %>%
  #print_quadro_resumo(var_name = ambulatorial_pc)

#box8 <- ggplot(ambulatorial) +
 # aes(x = as.factor(ano), y = ambulatorial_pc) +
  #geom_boxplot(fill = "#A11D21", width = 0.5) +
  #stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  #labs(x = "Ano", y = "Produção ambulatorial per capita") +
  #theme_estat(discreto = TRUE)
#box8

ncol_total <- ncol(df)
amb_cols <- (ncol_total - 4):ncol_total

ambulatorial <- df[2:nrow(df), amb_cols]
colnames(ambulatorial) <- c("2024", "2023", "2022", "2021", "2020")

# 2. Transformar para formato longo e tratar os dados
ambulatorial_long <- ambulatorial %>%
  pivot_longer(
    cols = everything(),
    names_to = "ano",
    values_to = "ambulatorial_pc"
  ) %>%
  # Converter para numérico e REMOVER NAs (essencial para não dar erro no boxplot)
  mutate(ambulatorial_pc = as.numeric(ambulatorial_pc)) %>%
  filter(!is.na(ambulatorial_pc))

# 3. Gerar o Quadro Resumo (Certifique-se de usar o dataframe novo 'ambulatorial_long')
ambulatorial_long %>%
  group_by(ano) %>%
  print_quadro_resumo(var_name = ambulatorial_pc)

# 4. Gerar o Gráfico
box8 <- ggplot(ambulatorial_long) + # Usar o df que tem a coluna 'ano'
  aes(x = as.factor(ano), y = ambulatorial_pc) +
  geom_boxplot(fill = "#A11D21", width = 0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Ano", y = "Produção ambulatorial per capita") +
  theme_estat(discreto = TRUE)

box8
