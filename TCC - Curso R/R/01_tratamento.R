library(tidyverse)
library(lubridate)

imdb <- readRDS('data-raw/rds/imdb.rds')
imdb_completa <- readRDS('data-raw/rds/imdb_completa.rds')

imdb_completa
imdb_data

# 1.  Qual o mês do ano com o maior númedo de filmes? E o dia do ano?


# Primeiramente escolhemos as colunas, transformando-as em datas,
# e retiramos os valores nulos desta coluna.

imdb_completa <- imdb_completa |>
  select(titulo, data_lancamento) |>
  mutate(data_lancamento = as.Date(data_lancamento)) |>
  drop_na (data_lancamento)


# O segundo passo é dividirmos em três a coluna relativa à data.


imdb_data <- imdb_completa |>
  mutate (ano = lubridate::year(data_lancamento),
          mes = lubridate::month(data_lancamento),
          dia = lubridate::day(data_lancamento))


# É possível responder à pergunta inicial de duas formas:
# a sucinta e a mais completa. vamos a elas?

# A partir de uma função criada sobre a qual  calculamos a moda,
# o termo mais repetido dentro de um vetor, temos:


mais_repetido(imdb_data$mes)

# Assim, o mês do ano com o maior número de filmes lançados
# dentre todos os filmes cuja data de estreia é conhecida
# é o décimo mês do ano, o popular outubro!

# Mas quantos filmes, dentre todos os 81.292 cuja data de lançamento
# é conhecida, foram lançados no mês de outubro, segundo o IMDB?



imdb_outubro <- imdb_data |>
  filmes_lancados_mensal() |>
  filter(mes_max==max(mes_max))

imdb_outubro

# Acima, agrupamos o nosso conjunto de dados pela coluna 'mes' e incluímos
# uma nova coluna, que nós dá o número de vezes em que há estreias,
# filtrando apenas para valores de outubro, o mês em que mais aparecem.
# Assim, pode-se concluir que 8.734 filmes foram lançados
# no mês de outubro, segundo o IMDB.

imdb_data2

######## ENCONTRAR MAIORES DE CADA MES

# Neste passo, usamos novamente a função que nós dá
# a estreia de cada filmes por todos os meses
# que compõem o ano. Assim, temos:

imdb_data_3 <- imdb_data |>
  filmes_lancados_mensal ()


imdb_data_3
# TABELA COM MES e N DE FILMES

imdb_data_3 |>
  distinct(mes, mes_max ) |>
  arrange(desc(mes_max)) |>
  mutate (filmes_lancados = mes_max) |>
  select(mes, filmes_lancados)
