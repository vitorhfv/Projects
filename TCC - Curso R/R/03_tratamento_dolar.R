library(tidyverse)

imdb_completa <- readRDS("data-raw/rds/imdb_completa.rds")

#4.  Considerando apenas orçamentos e receitas em dólar (\$),
#   qual o gênero com maior lucro? E com maior nota média?

# Primeiramente escolhemos colunas de interesse e descartamos NA.

regex_dolar = "^[\\$]"
regex_num = "\\(?[0-9,.]+"

imdb_orcam_rec <- imdb_completa |>
  select(titulo_original, ano, data_lancamento, nota_imdb, direcao, elenco, genero, orcamento, receita) |>
  drop_na(c("orcamento", "receita")) |>
  filter (str_detect(orcamento, regex_dolar)) |>
  mutate(orcamento = as.double(str_extract(orcamento, regex_num)),
         receita = as.double(str_extract(receita, regex_num)),
         lucro = receita - orcamento)


# Primeiramente filtramos a coluna de orçamento para valores somente
# que contenham dólar como moeda. Como a coluna de receita abarca apenas
# valores em dólar, temos duas colunas padronizadas neste momento.

# Com o mutate, transformamos ambas as colunas em categorias númericas
# flutuantes, enquanto extraímos, a partir de um código regex, valores entre
# 0-9. Por último, criamos a coluna de lucro.


# priceR

# Ajustamos o lucro a valores reais de 2020 com o priceR. Como o processo é demorado,
# preferiu-se no relatório apenas carregar um .rds com as alterações já implementadas.
# Abaixo tem-se o uso da biblioteca. A partir da função adjust_for_inflation,
# deflaciona-se a coluna lucro de cada filme a partir do ano de cada lançamento.
# Por fim, escolhe-se o ano para qual os valores são atualizados. Como a tibble
# do IMDB termina em 2020, este ano foi escolhido.

imdb_orcam_rec$lucro_real <-
  priceR::adjust_for_inflation(imdb_orcam_rec$lucro,
                               imdb_orcam_rec$ano,
                               "US",
                               to_date = 2020)


saveRDS(imdb_orcam_rec, "data/rds/imdb_orc_rec.rds")

imdb_orcam_rec <- readRDS("data/rds/imdb_orc_rec.rds")


genero_lucro_real <- imdb_orcam_rec |>
  separate_rows(genero, sep = "\\, ") |>
  group_by(genero) |>
  summarise(lucro_soma_genero = sum(lucro_real)) |>
  arrange(desc(lucro_soma_genero)) |>
  slice(1:5) |>
  mutate(genero = c("Aventura",
                    "Ação",
                    "Comédia",
                    "Drama",
                    "Animação"))

grafico_lucro_generos <- genero_lucro_real |>
  mutate(genero = forcats::fct_reorder(genero, lucro_soma_genero)) |>
  ggplot()+
  aes(x = lucro_soma_genero, y = genero) +
  geom_col ()+
  scale_x_continuous (labels = scales::dollar_format())+
  labs (title = "Os gêneros mais lucrativos do cinema",
        x = "lucro total acumulado (em dólares americanos)",
        y = "gêneros",
        caption = "Fonte: IMDB")

grafico_lucro_generos

#```{r}
#grafico_lucro_generos + theme_bw()
#```

scale_x_
# A partir do lucro real podemos brincar um pouco.
# Quais os filmes com o maior lucro real da história, em números absolutos,
# baseando-se apenas em valores em dólar?

imdb_orcam_rec |>
  select(titulo_original, ano, direcao, lucro_real) |>
  arrange(desc(lucro_real))


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
  filter (filmes_db > 1000) |>
  arrange(desc(media_imdb)) |>
  mutate(media_imdb = round(media_imdb, 2)) |>
  slice(1:5)

imdb_genero_nota
## GGPLOT

imdb_genero_nota |>
  mutate(media_imdb = forcats::as_factor(media_imdb),
         genero = c("Drama", "Crime", "Romance", "Aventura", "Suspense")) |>
  ggplot()+
  aes(x = genero, y = media_imdb)+
  geom_col()+
  labs(x = "gênero",
       y = "nota média do IMDB",
       caption = "Fonte: IMDB")




# Qual a maior bilheteria do IMDB, apenas valores em dólar?

# Primeiramente, carrega-se a base de dados. Em seguida, cria-se uma nova coluna
# para a base principal. Então, a partir de vetores, escolhemos as colunas de receita
# e ano de cada filme. O terceiro argumento é a moeda. Como falamos de dólares,
# coloca-se "US". Por fim, coloca-se o ano para o qual os dados serão deflacionados.
# Como o último ano da base é 2020, colocaremos este ano.

imdb_orcam_rec <- readRDS("data/rds/imdb_orc_rec.rds")

imdb_orcam_rec$receita_real <-
  priceR::adjust_for_inflation(imdb_orcam_rec$receita,
                               imdb_orcam_rec$ano,
                               "US",
                               to_date = 2020)

imdb_receita_real <- imdb_orcam_rec |>
  select(titulo_original, ano, receita_real)

saveRDS(imdb_receita_real, "data/rds/imdb_receita_real.rds")

imdb_receita_real <- readRDS("data/rds/imdb_receita_real.rds")

imdb_receita_real_f <- imdb_receita_real |>
  arrange(desc(receita_real)) |>
  slice (1:10) |>
  mutate(receita_real = scales::dollar(receita_real)) |>
  rename(titulo = titulo_original, receita = receita_real)

knitr::kable(imdb_receita_real_f)
