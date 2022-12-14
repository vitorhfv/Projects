library(tidyverse)


imdb_completa <- basesCursoR::pegar_base("imdb_completa")
imdb_pessoas <- basesCursoR::pegar_base("imdb_pessoas")
lucro_real <- readRDS("data\\rds\\lucro_real.rds")

# Dentre os filmes na base `imdb_completa`, escolha o seu favorito.
# Então faça os itens a seguir:
# a)  Quem dirigiu o filme? Faça uma ficha dessa pessoa:
# idade (hoje em dia ou data de falecimento), onde nasceu,
# quantos filmes já dirigiu,
# qual o lucro médio dos filmes que dirigiu (considerando apenas valores em dólar)
# e outras informações que achar interessante (base `imdb_pessoas`).

# A FELICIDADE NÃO SE COMPRA:  RANKING IMDB
afnsc_imdb <- imdb_completa |>
  select(titulo_original, ano, nota_imdb, num_avaliacoes) |>
  filter(num_avaliacoes > 50000) |>
  arrange(desc(nota_imdb)) |>
  slice(1:25) |>
  rownames_to_column("ranking") |>
  tail() |>
  rename(titulo = titulo_original,
         nota = nota_imdb,
         avaliacoes = num_avaliacoes)

afnsc_imdb

# Próx passo:
# Agora busquemos a respeito do diretor do filme.

capra_pessoa <- imdb_pessoas |>
  filter (nome == "Frank Capra") |>
  mutate(local_falecimento = "La Quinta, California, USA")


# diferença morte e nascimento

nascimento = lubridate::as_date(capra_pessoa$data_nascimento)
falecimento = lubridate::as_date(capra_pessoa$data_falecimento)
falecimento1 = as.character(falecimento)
intervalo <- lubridate::interval(start = nascimento, end = falecimento)
resultado <- lubridate::as.period(intervalo, unit = "year")
resultado
resultado1 = as.character(resultado)
# PERCENTIL

imdb_comp_percentil <- purrr::map(imdb_completa, function(x)
  paste0(ntile(x, n = 20L) / 20 * 100)) %>%
  as.data.frame()

# Adicionamos percentil na database principal do IMDB

imdb_completa$percentil_imdb <- imdb_comp_percentil$nota_imdb

capra_imdb <- imdb_completa |>
  select(titulo_original, ano, direcao, nota_imdb, percentil_imdb, elenco) |>
  filter(str_detect(direcao, "Frank Capra")) |>
  arrange(desc(nota_imdb))


capra_imdb_cresc <- capra_imdb |>
  arrange(desc(nota_imdb))

capra_imdb_cresc

nrow(capra_imdb)

# Lucro medio diretor
# Façamos uma tibble a respeito do lucro medio em usd de todos os diretores.

media_lucro_real_dir_real <- lucro_real |>
  separate_rows(direcao, sep = "\\, ") |>
  group_by(direcao) |>
  summarise(media_lucro_real = mean(lucro_real),
            filmes = n()) |>
  filter(filmes > 0) |>
  arrange(desc(media_lucro_real)) |>
  rownames_to_column("ranking") |>
  mutate(media_lucro_real = scales::dollar(media_lucro_real))

media_lucro_real_dir_real

media_lucro_capra <- media_lucro_real_dir_real |>
  filter(direcao == "Frank Capra")

media_lucro_capra
# ranking de lucro por filme para o caso do diretor em específico

lucro_ranking <- lucro_real |>
  arrange(desc(lucro_real)) |>
  rownames_to_column("ranking") |>
  select(ranking, titulo_original, ano, nota_imdb, direcao, elenco, genero, lucro_real)

lucro_ranking_capra <- lucro_ranking |>
  filter(direcao == "Frank Capra") |>
  mutate(lucro_real = scales::dollar(lucro_real))

# Ranking de filme favorito por lucro real.

lucro_ranking_capra
