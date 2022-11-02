library(tidyverse)

imdb_completa <- readRDS('data-raw/rds/imdb_completa.rds')

#4.  Considerando apenas orçamentos e receitas em dólar (\$),
#   qual o gênero com maior lucro? E com maior nota média?

# Primeiramente escolhemos colunas de interesse e descartamos NA.

imdb_orcam_rec <- imdb_completa |>
  select(titulo_original, ano, nota_imdb, genero, orcamento, receita) |>
  drop_na(c('orcamento', 'receita'))

#

regex_dolar = '^[\\$]'
regex_num = '\\(?[0-9,.]+'

# Primeiramente filtramos a coluna de orçamento para valores somente
# que contenham dólar como moeda. Como a coluna de receita abarca apenas
# valores em dólar, temos duas colunas padronizadas neste momento.

# Com o mutate, transformamos ambas as colunas em categorias númericas
# flutuantes, enquanto extraímos, a partir de um código regex, valores entre
# 0-9. Por último, criamos a coluna de lucro.


imdb_orcam_rec <- imdb_orcam_rec |>
  filter (str_detect(orcamento, regex_dolar)) |>
  mutate(orcamento = as.double(str_extract(orcamento, regex_num)),
         receita = as.double(str_extract(receita, regex_num)),
         lucro = receita - orcamento)

imdb_orcam_rec

# Separamos os gêneros de modo que ficassem empilhados. Assim, cada filme
# contará de forma separada para cada gênero. Isto posto, agrupamos a nova
# tibble por gênero e sumarizamos o lucro a partir de sua soma, de forma que
# tenhamos a soma do lucro de todos os filmes agrupados por gênero.

genero_lucro <- imdb_orcam_rec |>
  separate_rows(genero, sep = "\\, ") |>
  group_by(genero) |>
  summarise(lucro_soma_genero = sum(lucro)) %>%
  arrange(desc(lucro_soma_genero))

genero_lucro

# priceR

ano <- imdb_orcam_rec$ano
lucro <- imdb_orcam_rec$lucro


imdb_orcam_rec$lucro_real <-
  priceR::adjust_for_inflation(lucro, ano, 'US', to_date = 2020)


genero_lucro_real <- imdb_orcam_rec |>
  separate_rows(genero, sep = "\\, ") |>
  group_by(genero) |>
  summarise(lucro_soma_genero = sum(lucro_real)) %>%
  arrange(desc(lucro_soma_genero))

# Adiciona-se a coluna nova referente ao lucro real

genero_lucro$lucro_real <- genero_lucro_real$lucro_soma_genero

genero_lucro

# Não há mudanças! mas a partir do lucro real podemos brincar um pouco.
# Quais os filmes com o maior lucro real da história, em números absolutos,
# baseando-se apenas em valores em dólar?

imdb_orcam_rec |>
  select(titulo_original, ano, lucro_real) |>
  arrange(desc(lucro_real))

# Salvar a nova tibble em rds
saveRDS(imdb_orcam_rec, 'data\\rds\\lucro_real.rds')

# Qual a maior nota média por gênero a partir daqueles filmes cujo orçamento
# e receita estão em dólar?

# Reaproveita-se a tibble do exercício anterior e selecionamos as colunas de
# interesse, voltando a separar a coluna de gênero por empilhamento.
# Agrupa-se por gênero e se condensa a tibble pela média do imdb. Nota-se que
# escolhe-se apenas gêneros que contenham mais do que cinco filmes, de modo que
# não haja gênero com valor ínfimo.

imdb_genero_nota <- imdb_orcam_rec |>
  select(titulo_original, ano, nota_imdb, genero) |>
  separate_rows(genero, sep = "\\, ") |>
  group_by(genero) |>
  summarise(media_imdb = mean(nota_imdb),
            filmes_db = n ()) |>
  filter (filmes_db > 5) |>
  arrange(desc(media_imdb))

imdb_genero_nota

#

