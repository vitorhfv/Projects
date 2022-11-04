library(tidyverse)
library(ggplot2)

imdb_avaliacoes <- readRDS('data-raw/rds/imdb_avaliacoes.rds')
imdb_completa <- readRDS('data-raw/rds/imdb_completa.rds')

imdb_avaliacoes

# d)  Faça um gráfico representando a distribuição da nota atribuída
#     a esse filme por idade (base `imdb_avaliacoes`).

imdb_afnsc <- imdb_completa |>
  select(id_filme, titulo_original, direcao, ano) |>
  filter(direcao == 'Frank Capra',
         ano == '1946')


imdb_avaliacoes_fav <- imdb_avaliacoes |>
  filter(id_filme == imdb_afnsc$id_filme) |>
  select(num_votos_idade_0_18, nota_media_idade_0_18,
         num_votos_idade_18_30, nota_media_idade_18_30,
         num_votos_idade_30_45,
         num_votos_idade_45_mais)

imdb_avaliacoes_fav |>
  ggplot(aes (x= num_votos_idade_0_18, num_votos_idade_18_30, y= nota_media_idade_0_18))+
  geom_col()
