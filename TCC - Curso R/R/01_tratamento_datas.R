library(tidyverse)
library(lubridate)

imdb <- readRDS('data-raw/rds/imdb.rds')
imdb_completa <- readRDS('data-raw/rds/imdb_completa.rds')


# 1.  Qual o mês do ano com o maior número de filmes? E o dia do ano?


# Primeiramente escolhemos as colunas, transformando-as em datas,
# e retiramos os valores nulos desta coluna.

imdb_completa2 <- imdb_completa |>
  select(titulo, data_lancamento) |>
  mutate(data_lancamento = as.Date(data_lancamento)) |>
  drop_na (data_lancamento)


# O segundo passo é dividirmos em três a coluna relativa à data.


imdb_data <- imdb_completa2 |>
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


imdb_meses <- imdb_data |>
  filmes_lancados_total (mes) |>
  arrange(desc(filmes))


imdb_meses
#########

# Organizando uma tibble

imdb_meses_tibble <- imdb_meses |>
  distinct(mes, filmes) |>
  arrange(desc(filmes)) |>
  mutate (mes = lubridate::month(mes, label = TRUE),
    filmes_lancados = filmes) |>
  select(mes, filmes_lancados)

imdb_meses_tibble
# A pergunta subsequente é a seguinte:
# Qual o dia do ano em que há mais estreias cinematográficas?

imdb_data2 <- imdb_data |>
  unite('mes_dia', mes:dia, sep = '-') |>
  select(titulo, ano, mes_dia)

#

imdb_mes_dia <- imdb_data2 |>
  filmes_lancados_total(mes_dia) |>
  arrange(desc(filmes))

imdb_mes_dia
#

# Tibble geral
imdb_mes_dia_tibble<- imdb_mes_dia |>
  distinct(mes_dia, filmes ) |>
  separate(mes_dia, c('mes', 'dia')) |>
  arrange(desc(filmes))

# Transformamos a coluna relativa ao mês em 'double' para que
# possa ser transformada em data

imdb_mes_dia_tibble <- imdb_mes_dia_tibble |>
  mutate(mes = as.double(mes),
         filmes_estreia_total = filmes) |>
  select(mes, dia, filmes_estreia_total)

# Usamos o lubridate para que a coluna mês tenha o respectivo
# nome de cada mês.

imdb_mes_dia_tibble <- imdb_mes_dia_tibble |>
  mutate(mes = lubridate::month(mes, label = TRUE))

imdb_mes_dia_tibble

# Portanto, de todas as datas dentro um ano o 1º de outubro é aquela
# que comporta mais estreias cinematográficas! Porém, segundo a tibble
# 'imdb_mes_dia_tibble', vista acima, as dez maiores datas de estreia
# são aquelas relativas ao primeiro dia de um mês. Não é um comportamento
# estranho?

# É possível que, dentro do IMDB, o primeiro dia de um mês seja padrão para
# aquelas estreias cujo o dia é desconhecido, ainda que se conheça
# o mês de lançamento. Assim, filtraremos apenas o dia de estreia,
# de modo que seja maior do que 1 e contabilize todos os demais dias.

#### DIA > 1

imdb_mes_dia_tibble_mqu <- imdb_mes_dia_tibble |>
  filter (dia > 1)

imdb_mes_dia_tibble_mqu

# Agora sim! Das cinco maiores datas de estreia dentro de um ano,
# excluindo o primeiro dia de cada mês, há dois importantes feriados,
# dentre os quais o dia de Natal e o dia de São Valentim, quando se comemora
# o Dia dos Namorados em muitas partes do mundo.
# O primeiro lugar é o dia das Bruxas, o popular Halloween,
# também comemorado mundialmente no dia 31 de outubro.

# Será que há correlação entre a receita de um filme e o fato de sua estreia
# acontecer em um feriado
