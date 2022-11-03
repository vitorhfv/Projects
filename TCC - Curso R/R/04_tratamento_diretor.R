library(tidyverse)

imdb_completa <- readRDS('data-raw/rds/imdb_completa.rds')
imdb_pessoas <- readRDS('data-raw/rds/imdb_pessoas.rds')

# Dentre os filmes na base `imdb_completa`, escolha o seu favorito.
# Então faça os itens a seguir:
# a)  Quem dirigiu o filme? Faça uma ficha dessa pessoa:
# idade (hoje em dia ou data de falecimento), onde nasceu,
# quantos filmes já dirigiu,
# qual o lucro médio dos filmes que dirigiu (considerando apenas valores em dólar)
# e outras informações que achar interessante (base `imdb_pessoas`).

capra_pessoa <- imdb_pessoas |>
  filter (nome == 'Frank Capra')


# PERCENTIL

imdb_comp_percentil <- purrr::map(imdb_completa, function(x)
  paste0(ntile(x, n = 20L) / 20 * 100, "th percentile")) %>%
  as.data.frame()

# Adicionamos percentil na database principal do IMDB

imdb_completa$percentil_imdb <- imdb_comp_percentil$nota_imdb

#

capra_completo <- imdb_completa |>
  filter(str_detect(direcao, 'Frank Capra'))

#

capra_imdb <- capra_completo |>
  select(titulo_original, ano, nota_imdb, percentil_imdb, elenco)

capra_imdb

# Lucro medio diretor

lucro_real <- readRDS('data\\rds\\lucro_real.rds')

lucro_real_capra <- lucro_real |>
  filter(str_detect(direcao, 'Frank Capra'))

media_lucro_real_dir <- lucro_real |>
  separate_rows(direcao, sep = "\\, ") |>
  group_by(direcao) |>
  summarise(media_lucro_real = mean(lucro_real),
            filmes = n()) |>
  filter(filmes > 1) |>
  arrange(desc(media_lucro_real)) |>
  rownames_to_column('ranking')


media_lucro_real_dir

lucro_real_dir_ranking |>
  filter(direcao == 'Frank Capra')
