library(tidyverse)

imdb_completa <- readRDS('data-raw/rds/imdb_completa.rds')

# Primeiramente selecionamos as colunas de interesse, dividimos a coluna de
# países e retiramos os valores nulos.
#

imdb_paises <- imdb_completa |>
  select(titulo_original, ano, pais) |>
  separate_rows(pais, sep = '\\, ') |>
  drop_na(pais)


# Voltamos a usar a função que agrupa o número total de filmes a partir
# de uma coluna. No caso, agrupamos por país. Usamos a função distinct
# para reorganizar a tibble a partir das colunas de países
# e a contagem de filmes.


imdb_paises |>
  filmes_lancados_total(pais) |>
  distinct(pais, filmes) |>
  arrange(desc(filmes)) |>
  head (5)

####### COMENTAR TOP 5

###### NUMERO DE MOEDAS EM ORCAMENTO \ RECEITA

imdb_orcrec <- imdb_completa |>
  select(titulo_original, ano, orcamento, receita) |>
  drop_na (c('orcamento', 'receita'))

# REGEX ENCONTRA

regex = "[A-Z|\\$]+(?=\\s)"

# A-Z maicusculo, |, caractere monetário

imdb_moedas <- imdb_orcrec |>
  mutate(moeda_orc = str_extract(orcamento, regex),
         moeda_rec = str_extract(receita, regex)) |>
  select(titulo_original, ano, moeda_orc, moeda_rec)

imdb_moedas

imdb_orcamento <- imdb_moedas |>
  filmes_lancados_total(moeda_orc) |>
  distinct(moeda_orc, filmes) |>
  arrange(desc(filmes))

imdb_receita <- imdb_moedas |>
  filmes_lancados_total(moeda_rec) |>
  distinct(moeda_rec, filmes)


imdb_orcamento
# Há 62 moedas.

############## 4

imdb_moeda_dolar <- imdb_moedas |>
  filter(moeda_orc == '$')

imdb_moeda_dolar

#
# Verificar se colunas de receita e orçamento são iguais
all(imdb_moeda_dolar$moeda_orc == imdb_moeda_dolar$moeda_rec)
