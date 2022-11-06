library(tidyverse)

imdb_avaliacoes <- readRDS('data-raw/rds/imdb_avaliacoes.rds')
imdb_completa <- readRDS('data-raw/rds/imdb_completa.rds')

imdb_avaliacoes

# d)  Faça um gráfico representando a distribuição da nota atribuída
#     a esse filme por idade (base `imdb_avaliacoes`).

imdb_afnsc <- imdb_completa |>
  select(id_filme, titulo_original, direcao, ano) |>
  filter(direcao == 'Frank Capra',
         ano == '1946')

# gráficp

imdb_avaliacoes_fav <- imdb_avaliacoes |>
  filter(id_filme == imdb_afnsc$id_filme) |>
  pivot_longer(cols = -1)


imdb_avaliacoes_fav <- imdb_avaliacoes_fav |>
  filter(str_detect(name, 'nota_media_idade')) |>
  mutate(name = str_remove_all(name, '^\\D+'))

fav_imdb_final <- imdb_avaliacoes_fav |>
  rename(idade = name, nota = value) |>
  mutate(across(
    .cols = idade,
    ~str_replace( ., "_", "-" )
  ))


fav_imdb_final <- fav_imdb_final |>
  mutate(idade = forcats::fct_recode(idade,
                                     'Adolescentes' = '0-18',
                                     'Jovens' = '18-30',
                                     'Adultos' = '30-45',
                                     'Pré-idosos' = '45-mais'
                                     ))

fav_imdb_final

fav_imdb_final |>
  ggplot()+
  ggtitle('As notas de A Felicidade Não se Compra (1946) por faixa etária')+
  aes(x = idade, y = nota)+
  geom_col()+
  coord_cartesian(ylim = c(8.3, 8.8))+
  scale_y_continuous(breaks = seq(8.3, 8.8, by = 0.1))


