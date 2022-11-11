# Função para encontrarmos o valor mais repetido
# dentro de um vetor. É a popular moda da matemática,
# em que 'x' é o vetor.

mais_repetido <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}


# Função que agrupa e nos dá uma coluna nova
# relativa ao número de filmes lançados baseados em uma coluna
# a ser escolhida (mês, dia, mês_dia, nacionalidade etc)


filmes_lancados_total<- function (data, by, ...) {
  data %>%
    group_by({{by}}) |>
    mutate(filmes=n()) |>
    ungroup()
}

floor_decade  = function(value){ return(value - value %% 10) }
