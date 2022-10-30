# Função para encontrarmos o valor mais repetido
# dentro de um vetor. É a popular moda da matemática,
# em que 'x' é o vetor.

mais_repetido <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

