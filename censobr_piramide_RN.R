# ===========================================================
#  PIRÂMIDE ETÁRIA DO RN (Censo 2022) COM O {censobr}
#  Seções 1 a 4: Giuliano | Seções 5 a 9: Gabriel
# ===========================================================


# 1. PACOTES ------------------------------------------------
library(censobr)
library(dplyr)
library(ggplot2)

# acentos certos no gráfico: sem isto, a sessão sem UTF-8
# escrevia "Pir..mide et..ria" no título
for (loc in c("pt_BR.UTF-8", "C.UTF-8", "en_US.UTF-8")) {
  if (suppressWarnings(Sys.setlocale("LC_CTYPE", loc)) != "") break
}

# função para mostrar cada gráfico numa janela separada:
# abre a janela, desenha, espera o Enter e fecha
mostrar_grafico <- function(grafico, largura = 10, altura = 7) {
  tryCatch(x11(width = largura, height = altura),   # abre a janela do gráfico
           error = function(e) dev.new())            # se não der, usa o padrão
  print(grafico)                               # desenha o gráfico nela
  readline("Pressione Enter para fechar o gráfico... ")
  dev.off()                                    # fecha a janela
  invisible(NULL)
}

zip_ibge <- "microdados_RN_2022.zip"   # só a pasta 24 (RN) do zip do IBGE
uf_alvo  <- "RN"


# 2. DOCUMENTAÇÃO -------------------------------------------
# dicionário de variáveis, questionário e manual do recenseador
data_dictionary(year = 2022, dataset = "microdata")
questionnaire(year = 2022, type = "long")
interview_manual(year = 2022)


# 3. IMPORTAR O ZIP DO IBGE (só na primeira vez) ------------
# converte os CSVs do zip em Parquet no cache, com os tipos certos
ja_importado <- any(grepl("2022_population.controlado",
                          list.files(get_censobr_cache_dir(), recursive = TRUE)))

if (!ja_importado) import_microdata22(zip_path = zip_ibge)


# 4. LER AS PESSOAS -----------------------------------------
# P0160 = sexo | P0181 = idade | P0111 = peso amostral
# para outro Censo: troque o year e os códigos (ver data_dictionary)
pessoas <- read_population(
  year       = 2022,
  columns    = c("abbrev_state", "P0160", "P0181", "P0111"),
  add_labels = "pt"
)
pessoas   # ainda é uma consulta: nada foi para a memória


# 5. FILTRAR O RN E CONTAR A POPULAÇÃO ----------------------
pessoas_rn <- pessoas |>
  filter(abbrev_state == uf_alvo)

pessoas_rn |>
  summarise(populacao = sum(P0111)) |>
  collect()   # aqui a consulta roda e o resultado vem para a memória


# 6. PIRÂMIDE ETÁRIA (EM %) ---------------------------------

# 6.1 População por sexo e idade simples (soma dos pesos)
populacao_idade <- pessoas_rn |>
  group_by(Sexo = P0160, Idade = P0181) |>        # uma linha por sexo e idade
  summarise(Populacao = sum(P0111), .groups = "drop") |>
  collect()                                       # cerca de 200 linhas vêm para a memória

# 6.2 Faixas etárias de 5 anos
populacao_idade$Faixa_Etaria <- cut(              # cria as faixas
  populacao_idade$Idade,                          # variável que será dividida
  breaks = c(seq(0, 90, by = 5), Inf),            # quebras: 0, 5, 10, ..., 90 e mais
  labels = c(paste(seq(0, 85, by = 5),            # rótulos: 0-4, 5-9, ..., 85-89
                   seq(4, 89, by = 5), sep = "-"),
             "90+"),                              # última faixa: 90 anos ou mais
  right = FALSE                                   # fecha à esquerda: 5 anos cai em 5-9
)

populacao_idade$Sexo <- factor(                   # define a ordem da legenda
  populacao_idade$Sexo,
  levels = c("Masculino", "Feminino")
)

# 6.3 Tabela da pirâmide: população por faixa e sexo
tabela_piramide <- populacao_idade |>
  group_by(Faixa_Etaria, Sexo) |>
  summarise(Populacao = sum(Populacao), .groups = "drop")

# 6.4 Percentual sobre a população TOTAL do RN (homens + mulheres = 100%)
# em %, a pirâmide pode ser comparada com a de qualquer outra UF
tabela_piramide$Percentual <- 100 * tabela_piramide$Populacao /
  sum(tabela_piramide$Populacao)

sum(tabela_piramide$Percentual)                   # conferência: tem que dar 100

# 6.5 Homens com valores negativos, para ficarem à esquerda
tabela_piramide$Valor <- ifelse(
  tabela_piramide$Sexo == "Masculino",            # se for homem
  -tabela_piramide$Percentual,                    # fica negativo
  tabela_piramide$Percentual                      # se for mulher, fica positivo
)

limite <- ceiling(max(tabela_piramide$Percentual)) # maior barra, arredondada para cima

# 6.6 Gráfico
grafico_piramide <- ggplot(
  tabela_piramide,                                # dados
  aes(x = Faixa_Etaria,                           # eixo x: faixas etárias
      y = Valor,                                  # eixo y: percentual
      fill = Sexo)                                # cor das barras
) +
  geom_col(width = 0.8) +                         # barras com 80% da largura
  coord_flip() +                                  # deita o gráfico: faixas na vertical
  scale_y_continuous(
    labels = function(x) paste0(abs(x), "%"),     # mostra 3% dos dois lados, sem sinal
    limits = c(-limite, limite),                  # escala simétrica
    breaks = seq(-limite, limite, by = 1)         # marca de 1 em 1%
  ) +
  scale_fill_manual(                              # cores por sexo
    values = c("Masculino" = "#3182bd", "Feminino" = "#de2d26")
  ) +
  labs(
    title    = "Pirâmide Etária do Rio Grande do Norte (2022)",
    subtitle = "Percentual da população total, por sexo e faixa etária",
    x        = "Faixa Etária",
    y        = "% da população",
    fill     = "Sexo",
    caption  = "Fonte: IBGE, Censo Demográfico 2022 (amostra), via censobr"
  ) +
  theme_minimal() +                               # tema limpo
  theme(
    plot.title    = element_text(hjust = 0.5, face = "bold"), # título centralizado e em negrito
    plot.subtitle = element_text(hjust = 0.5)                 # subtítulo centralizado
  )

mostrar_grafico(grafico_piramide)   # abre a janela; Enter fecha


# 6.7 Razão de dependência (indicador usado em previdência)
# dependentes (0-14 e 65+) para cada 100 pessoas em idade ativa (15-64)
jovens <- sum(populacao_idade$Populacao[populacao_idade$Idade < 15])
idosos <- sum(populacao_idade$Populacao[populacao_idade$Idade >= 65])
ativos <- sum(populacao_idade$Populacao[populacao_idade$Idade >= 15 &
                                        populacao_idade$Idade < 65])

round(100 * c(total  = (jovens + idosos) / ativos,
              jovens = jovens / ativos,
              idosos = idosos / ativos), 1)


# 7. DOMICÍLIOS E JUNÇÃO PESSOA-DOMICÍLIO ------------------
# D0330 = domicílio tem internet | D0111 = peso do domicílio

# 7.1 Domicílios com e sem internet
internet <- read_households(
  year       = 2022,
  columns    = c("abbrev_state", "D0330", "D0111"),
  add_labels = "pt"
) |>
  filter(abbrev_state == uf_alvo, !is.na(D0330)) |>
  group_by(Internet = D0330) |>
  summarise(Domicilios = sum(D0111)) |>
  collect()

internet$Percentual <- 100 * internet$Domicilios / sum(internet$Domicilios)
internet

grafico_internet <- ggplot(internet, aes(x = Internet, y = Percentual, fill = Internet)) +
  geom_col(width = 0.6) +                                   # uma barra por resposta
  geom_text(aes(label = paste0(round(Percentual, 1), "%")), # valor em cima da barra
            vjust = -0.5) +
  scale_y_continuous(limits = c(0, 105)) +                  # espaço para o rótulo no topo
  scale_fill_manual(values = c("Sim" = "#3182bd", "Não" = "#bdbdbd")) +
  labs(title = "Domicílios com acesso à internet no RN (2022)",
       x = "O domicílio tem internet?", y = "% dos domicílios") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none")                           # cores já explicadas no eixo

mostrar_grafico(grafico_internet)   # abre a janela; Enter fecha

# 7.2 Juntar pessoa + domicílio: um argumento (merge_households)
internet_sexo <- read_population(
  year             = 2022,
  columns          = c("abbrev_state", "P0160", "P0111", "D0330"),
  add_labels       = "pt",
  merge_households = TRUE                                   # chave P0100 = D0100
) |>
  filter(abbrev_state == uf_alvo, !is.na(D0330)) |>
  group_by(Sexo = P0160) |>
  summarise(Percentual = 100 * sum(if_else(D0330 == "Sim", P0111, 0)) / sum(P0111)) |>
  collect()

internet_sexo

grafico_internet_sexo <- ggplot(internet_sexo, aes(x = Sexo, y = Percentual, fill = Sexo)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(Percentual, 1), "%")), vjust = -0.5) +
  scale_y_continuous(limits = c(0, 105)) +
  scale_fill_manual(values = c("Masculino" = "#3182bd", "Feminino" = "#de2d26")) +
  labs(title = "Pessoas que moram em domicílio com internet, por sexo (RN, 2022)",
       x = "Sexo", y = "% das pessoas") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none")

mostrar_grafico(grafico_internet_sexo)   # abre a janela; Enter fecha


# 8. MORTALIDADE E FAMÍLIAS ---------------------------------
# M0150 = ano do óbito | M0160 = sexo de quem faleceu | M0111 = peso
# o Censo perguntou sobre óbitos de jan/2019 a jul/2022 (2022 está incompleto)

# 8.1 Óbitos por ano e sexo
obitos <- read_mortality(
  year       = 2022,
  columns    = c("abbrev_state", "M0150", "M0160", "M0111"),
  add_labels = "pt"
) |>
  filter(abbrev_state == uf_alvo,
         M0150 %in% c("2019", "2020", "2021", "2022"),      # tira "Ignorado"
         M0160 %in% c("Masculino", "Feminino")) |>
  group_by(Ano = M0150, Sexo = M0160) |>
  summarise(Obitos = sum(M0111)) |>
  collect()

obitos$Sexo <- factor(obitos$Sexo, levels = c("Masculino", "Feminino"))
obitos

grafico_obitos <- ggplot(obitos, aes(x = Ano, y = Obitos, fill = Sexo)) +
  geom_col(position = "dodge", width = 0.7) +               # barras lado a lado por sexo
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  scale_fill_manual(values = c("Masculino" = "#3182bd", "Feminino" = "#de2d26")) +
  labs(title    = "Óbitos informados no Censo, por ano e sexo (RN)",
       subtitle = "2022: apenas de janeiro a julho",
       x = "Ano do óbito", y = "Óbitos", fill = "Sexo") +
  theme_minimal() +
  theme(plot.title    = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

mostrar_grafico(grafico_obitos)   # abre a janela; Enter fecha

# 8.2 Total de famílias (F0111 = peso da família)
read_families(year = 2022, columns = c("abbrev_state", "F0111")) |>
  filter(abbrev_state == uf_alvo) |>
  summarise(Familias = sum(F0111)) |>
  collect()

# read_emigration() também existe, mas só para o Censo 2010


# 9. SETORES CENSITÁRIOS E CACHE ----------------------------
# V0001 = pessoas no setor | 24 = código do RN

# 9.1 Os 10 municípios mais populosos do RN
municipios <- read_tracts(year = 2022, dataset = "Basico") |>
  filter(code_state == 24) |>
  group_by(Municipio = name_muni) |>
  summarise(Populacao = sum(V0001, na.rm = TRUE)) |>
  arrange(desc(Populacao)) |>
  head(10) |>
  collect()

municipios

grafico_municipios <- ggplot(municipios, aes(x = reorder(Municipio, Populacao), y = Populacao)) +
  geom_col(fill = "#3182bd", width = 0.7) +
  coord_flip() +                                            # nomes na vertical, mais fáceis de ler
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  labs(title = "Os 10 municípios mais populosos do RN (2022)",
       x = "Município", y = "Pessoas") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

mostrar_grafico(grafico_municipios)   # abre a janela; Enter fecha

# 9.2 Cache: o pacote baixa cada arquivo uma vez e guarda no computador
censobr_cache(list_files = TRUE)    # lista o que já está guardado
get_censobr_cache_dir()             # mostra a pasta onde fica guardado
# set_censobr_cache_dir("outra/pasta")   # mudaria a pasta (não rodar na apresentação)
