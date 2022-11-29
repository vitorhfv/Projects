library(tidyverse)

imdb_completa <- basesCursoR::pegar_base("imdb_completa")

# c)  Em que dia A Felicidade Não se Compra foi lançado?

afnsc_ano_orig <- imdb_completa |>
  select(titulo_original, ano, direcao) |>
  filter(direcao == "Frank Capra",
         ano == "1946")

fav_data <- imdb_completa |>
  filter(direcao == "Frank Capra",
         ano == "1946") |>
  mutate(data_lancamento = lubridate::as_date(data_lancamento),
         dia_sem = lubridate::wday(data_lancamento, label = TRUE, abbr = FALSE),
         ano = lubridate::year(data_lancamento),
         mes = lubridate::month(data_lancamento),
         dia = lubridate::day(data_lancamento)) |>
  select(titulo_original, direcao, dia, mes, ano, dia_sem)

fav_data # 11, 3, 1948

#  Algum outro filme foi lançado no mesmo dia?

imdb_datas <- imdb_completa |>
  select(titulo_original, nota_imdb, direcao, ano, data_lancamento) |>
  mutate(data_lancamento = lubridate::as_date(data_lancamento)) |>
  drop_na(data_lancamento) |>
  mutate(dia_sem = lubridate::wday(data_lancamento, label = TRUE, abbr = FALSE),
         ano = lubridate::year(data_lancamento),
         mes = lubridate::month(data_lancamento),
         dia = lubridate::day(data_lancamento))

imdb_datas

imdb_datas_iguais <- imdb_datas |>
  select(titulo_original, nota_imdb, direcao, dia, mes, ano , dia_sem) |>
  filter(dia == "11",
         mes == "3",
         ano == "1948")


imdb_datas_iguais$titulo_original[2]
# MESMA DATA DE ESTREIA DE NESTE MUNDO E NO OUTRO.

#     Quantos anos você tinha nesse dia?

imdb_data_aniv <- imdb_completa |>
  select(titulo_original, ano, direcao, data_lancamento) |>
  filter(ano == "1946",
         direcao == "Frank Capra")

lancamento_fav = lubridate::as_date(imdb_data_aniv$data_lancamento)
aniversario = lubridate::as_date("1999-07-02")
intervalo <- lubridate::interval(start = lancamento_fav, end = aniversario)
resultado_f <- lubridate::as.period(intervalo, unit = "year")
resultado_f1 <- as.character(resultado_f)
