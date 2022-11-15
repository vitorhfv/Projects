library(tidyverse)

imdb_completa <- readRDS("data-raw/rds/imdb_completa.rds")

# Primeiramente selecionamos as colunas de interesse, dividimos a coluna de
# países e retiramos os valores nulos.
#

imdb_paises <- imdb_completa |>
  select(titulo_original, ano, pais) |>
  separate_rows(pais, sep = "\\, ") |>
  drop_na(pais) |>
  filmes_lancados_total(pais) |>
  distinct(pais, filmes) |>
  arrange(desc(filmes)) |>
  slice(1:100)

knitr::kable(imdb_paises)

# Voltamos a usar a função que agrupa o número total de filmes a partir
# de uma coluna. No caso, agrupamos por país. Usamos a função distinct
# para reorganizar a tibble a partir das colunas de países
# e a contagem de filmes.

scales::dollar(genero_lucro_real$lucro_soma_genero[[1]])

####### COMENTAR TOP 5

###### NUMERO DE MOEDAS EM ORCAMENTO \ RECEITA

imdb_orcrec <- imdb_completa |>
  select(titulo_original, ano, orcamento, receita) |>
  drop_na (c("orcamento", "receita"))

# REGEX PARA

regex = "[A-Z|\\$]+(?=\\s)"

# A-Z maicusculo, |, caractere monetário

imdb_moedas <- imdb_orcrec |>
  mutate(moeda_orc = str_extract(orcamento, regex),
         moeda_rec = str_extract(receita, regex)) |>
  select(titulo_original, ano, moeda_orc, moeda_rec)


imdb_orcamento <- imdb_moedas |>
  filmes_lancados_total(moeda_orc) |>
  distinct(moeda_orc, filmes) |>
  arrange(desc(filmes))

imdb_receita <- imdb_moedas |>
  filmes_lancados_total(moeda_rec) |>
  distinct(moeda_rec, filmes)

tabela_orc <- imdb_orcamento |>
  slice(1:3) |>
  rename(moeda = moeda_orc) |>
  mutate(regiao = c("Estados Unidos", "União Europeia", "Índia")) |>
  select(moeda, regiao, filmes)

knitr::kable(tabela_orc)

# Verificação de igualdade de orçamento e receita em n de filmes.

n_filmes_orc <- imdb_orcamento |>
  mutate(filmes_em_orcamento = sum(filmes),
         filmes_em_receita = sum(filmes)) |>
  select(filmes_em_orcamento, filmes_em_receita) |>
  distinct(filmes_em_orcamento, filmes_em_receita)

n_filmes_orc$filmes_em_orcamento == n_filmes_orc$filmes_em_receita

# Numero total de padrões monetários

imdb_orcamento

nrow(imdb_orcamento)

############## 4


