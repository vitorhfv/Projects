library(tidyverse)

imdb_completa <- basesCursoR::pegar_base("imdb_completa")

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

# A partir de uma função sobre a qual  calculamos a moda,
# o termo mais repetido dentro de um vetor, temos:


mais_repetido(imdb_data$mes)

# Assim, o mês do ano com o maior número de filmes lançados
# dentre todos os filmes cuja data de estreia é conhecida
# é o décimo mês do ano, o popular outubro!

# Mas quantos filmes, dentre todos os 81.292 cuja data de lançamento
# é conhecida, foram lançados no mês de outubro, segundo o IMDB?


imdb_meses <- imdb_data |>
  filmes_lancados_total (mes) |>
  distinct(mes, filmes) |>
  arrange(desc(filmes)) |>
  mutate (mes = lubridate::month(mes, label = TRUE)) |>
  select(mes, filmes)

imdb_meses
knitr::kable(imdb_meses)


imdb_meses |>
  ggplot()+
  aes(x = mes, y = filmes, group = 1)+
  geom_line(size = 1.6)+
  theme_set(theme_bw())

#########

# A pergunta subsequente é a seguinte:
# Qual o dia do ano em que há mais estreias cinematográficas?

imdb_data2 <- imdb_data |>
  unite("mes_dia", mes:dia, sep = "-") |>
  filmes_lancados_total(mes_dia) |>
  distinct(mes_dia, filmes ) |>
  separate(mes_dia, c("mes", "dia")) |>
  mutate(mes = as.double(mes),
         dia = as.double(dia),
         filmes_estreia_total = filmes) |>
  select(mes, dia, filmes) |>
  arrange(desc(filmes))

imdb_data2

# Transformamos a coluna relativa ao mês em "double" para que
# possa ser transformada em data

#

# Usamos o lubridate para que a coluna mês tenha o respectivo
# nome de cada mês.

imdb_mes_dia_tibble <- imdb_data2 |>
  mutate(mes = lubridate::month(mes, label = TRUE))

imdb_mes_dia_tibble

knitr::kable(imdb_mes_dia_tibble_mqu)

# Portanto, de todas as datas dentro um ano o 1º de outubro é aquela
# que comporta mais estreias cinematográficas! Porém, segundo a tibble
# "imdb_mes_dia_tibble", vista acima, as dez maiores datas de estreia
# são aquelas relativas ao primeiro dia de um mês. Não é um comportamento
# estranho?

# É possível que, dentro do IMDB, o primeiro dia de um mês seja padrão para
# aquelas estreias cujo o dia é desconhecido, ainda que se conheça
# o mês de lançamento. Assim, filtraremos apenas o dia de estreia,
# de modo que seja maior do que 1, contabilizando somente os demais dias.

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

imdb_decadas <- imdb_data |>
  mutate(decada = floor_decade(ano)) |>
  select(titulo, decada) |>
  filmes_lancados_total(decada) |>
  distinct(decada, filmes) |>
  arrange(desc(filmes)) |>
  ggplot()

imdb_decadas

# Será que há correlação entre a receita de um filme e o fato de a sua estreia
# acontecer em um feriado?
#imdb_orcam_rec <- readRDS("data/rds/imdb_orcam_rec.rds")

#imdb_correla <- imdb_orcam_rec |>
  #select(titulo_original, data_lancamento, receita)
#

# imdb_correla$receita_real <-
  priceR::adjust_for_inflation(imdb_correla$receita,
                               imdb_correla$data_lancamento,
                               "US",
                               to_date = 2020)

# imdb_orcam_rec <- readRDS("data/rds/imdb_orcam_rec.rds")




#imdb_correla <- readRDS("data/rds/imdb_corr.rds")


imdb_correla

imdb_correla <- imdb_correla |>
  select(titulo_original, data_lancamento, receita_real) |>
  filter(data_lancamento > "1999")

imdb_correla



??holidays
