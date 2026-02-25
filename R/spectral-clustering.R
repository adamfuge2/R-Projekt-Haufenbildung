# spectral clustering

df <- tibble::tibble(x = c(1,2,3), y = c(4, 5, 6))
df
tibble::as_tibble(as.matrix(dist(df)))

gamma <- 5
exp(- gamma * as.matrix(dist(df)))
